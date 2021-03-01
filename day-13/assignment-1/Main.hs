{- --- Day 13: Care Package ---
 -
 - As you ponder the solitude of space and the ever-increasing three-hour
 - roundtrip for messages between you and Earth, you notice that the Space Mail
 - Indicator Light is blinking. To help keep you sane, the Elves have sent you a
 - care package.
 -
 - It's a new game for the ship's arcade cabinet! Unfortunately, the arcade is
 - all the way on the other end of the ship. Surely, it won't be hard to build
 - your own - the care package even comes with schematics.
 -
 - The arcade cabinet runs Intcode software like the game the Elves sent (your
 - puzzle input). It has a primitive screen capable of drawing square tiles on a
 - grid. The software draws tiles to the screen with output instructions: every
 - three output instructions specify the x position (distance from the left), y
 - position (distance from the top), and tile id. The tile id is interpreted as
 - follows:
 -
 - * 0 is an empty tile. No game object appears in this tile.
 - * 1 is a wall tile. Walls are indestructible barriers.
 - * 2 is a block tile. Blocks can be broken by the ball.
 - * 3 is a horizontal paddle tile. The paddle is indestructible.
 - * 4 is a ball tile. The ball moves diagonally and bounces off objects.
 -
 - For example, a sequence of output values like 1,2,3,6,5,4 would draw a
 - horizontal paddle tile (1 tile from the left and 2 tiles from the top) and a
 - ball tile (6 tiles from the left and 5 tiles from the top).
 -
 - Start the game. How many block tiles are on the screen when the game exits?
 -}
module Main where

import AdventOfCode
import IntCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Control.Monad as C
import qualified Control.Monad.State as C
import qualified Control.Monad.Except as C
import qualified Control.Concurrent as C
import qualified Data.Set as Set
import qualified Data.Map as Map

main :: IO ()
main = defaultMain parseInput handleInput

data GameState = GameState (Map.Map Position GameTile) deriving (Show, Eq, Ord)

data GameTile = Empty | Wall | Block | HorizontalPaddle | Ball
    deriving (Show, Eq, Ord)

type Position = (Int, Int)

data Move = Move !Position GameTile deriving (Show, Eq, Ord)

handleInput :: Program -> IO ()
handleInput program = do
    cin <- C.newChan
    cout <- C.newChan
    let state = ExecutionState program 0 0 cin cout

    C.void $ C.forkIO (execute state)
    output <- allOutput cout
    case parseMoves output of
        Right moves -> run moves
        Left err -> print err

allOutput :: C.Chan ProgramOutput -> IO [Int]
allOutput cout = do
    output <- takeWhile (/= Halted) <$> C.getChanContents cout
    return $! map (\(ProgramOutput n) -> n) output

run :: [Move] -> IO ()
run moves = print blockN
  where
    (GameState board) = simulateGame moves
    blockN = length . filter (== Block) . Map.elems $ board

simulateGame :: [Move] -> GameState
simulateGame moves = foldl step (GameState Map.empty) moves

step :: GameState -> Move -> GameState
step (GameState board) (Move pos tile) = GameState board'
  where
    board' = Map.insert pos tile board

parseMoves :: [Int] -> Either P.ParseError [Move]
parseMoves = P.parse (P.many parseMove <* P.eof) ""

parseMove :: P.Parsec [Int] () Move
parseMove = Move <$> parsePosition <*> parseGameTile

parsePosition :: P.Parsec [Int] () Position
parsePosition = (,) <$> P.anyToken <*> P.anyToken

parseGameTile :: P.Parsec [Int] () GameTile
parseGameTile = tileFromInt <$> P.anyToken

tileFromInt :: Int -> GameTile
tileFromInt 0 = Empty
tileFromInt 1 = Wall
tileFromInt 2 = Block
tileFromInt 3 = HorizontalPaddle
tileFromInt 4 = Ball
tileFromInt n = error $ "Not valid tile number " ++ show n ++ "."

parseInput :: T.Text -> Either P.ParseError Program
parseInput = P.parse (parseProgram <* P.eof) ""
