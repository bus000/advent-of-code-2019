{- --- Day 2: 1202 Program Alarm ---
 -
 - On the way to your gravity assist around the Moon, your ship computer beeps
 - angrily about a "1202 program alarm". On the radio, an Elf is already
 - explaining how to handle the situation: "Don't worry, that's perfectly
 - norma--" The ship computer bursts into flames.
 -
 - You notify the Elves that the computer's magic smoke seems to have escaped.
 - "That computer ran Intcode programs like the gravity assist program it was
 - working on; surely there are enough spare parts up there to build a new
 - Intcode computer!"
 -
 - An Intcode program is a list of integers separated by commas (like
 - 1,0,0,3,99). To run one, start by looking at the first integer (called
 - position 0). Here, you will find an opcode - either 1, 2, or 99. The opcode
 - indicates what to do; for example, 99 means that the program is finished and
 - should immediately halt. Encountering an unknown opcode means something went
 - wrong.
 -
 - Opcode 1 adds together numbers read from two positions and stores the result
 - in a third position. The three integers immediately after the opcode tell you
 - these three positions - the first two indicate the positions from which you
 - should read the input values, and the third indicates the position at which
 - the output should be stored.
 -
 - For example, if your Intcode computer encounters 1,10,20,30, it should read
 - the values at positions 10 and 20, add those values, and then overwrite the
 - value at position 30 with their sum.
 -
 - Opcode 2 works exactly like opcode 1, except it multiplies the two inputs
 - instead of adding them. Again, the three integers after the opcode indicate
 - where the inputs and outputs are, not their values.
 -
 - Once you're done processing an opcode, move to the next one by stepping
 - forward 4 positions.
 -
 - For example, suppose you have the following program:
 -
 - 1,9,10,3,2,3,11,0,99,30,40,50
 -
 - For the purposes of illustration, here is the same program split into
 - multiple lines:
 -
 - 1,9,10,3,
 - 2,3,11,0,
 - 99,
 - 30,40,50
 -
 - The first four integers, 1,9,10,3, are at positions 0, 1, 2, and 3. Together,
 - they represent the first opcode (1, addition), the positions of the two
 - inputs (9 and 10), and the position of the output (3). To handle this opcode,
 - you first need to get the values at the input positions: position 9 contains
 - 30, and position 10 contains 40. Add these numbers together to get 70. Then,
 - store this value at the output position; here, the output position (3) is at
 - position 3, so it overwrites itself. Afterward, the program looks like this:
 -
 - 1,9,10,70,
 - 2,3,11,0,
 - 99,
 - 30,40,50
 -
 - Step forward 4 positions to reach the next opcode, 2. This opcode works just
 - like the previous, but it multiplies instead of adding. The inputs are at
 - positions 3 and 11; these positions contain 70 and 50 respectively.
 - Multiplying these produces 3500; this is stored at position 0:
 -
 - 3500,9,10,70,
 - 2,3,11,0,
 - 99,
 - 30,40,50
 -
 - Stepping forward 4 more positions arrives at opcode 99, halting the program.
 -
 - Here are the initial and final states of a few more small programs:
 -
 - * 1,0,0,0,99 becomes 2,0,0,0,99 (1 + 1 = 2).
 - * 2,3,0,3,99 becomes 2,3,0,6,99 (3 * 2 = 6).
 - * 2,4,4,5,99,0 becomes 2,4,4,5,99,9801 (99 * 99 = 9801).
 - * 1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99.
 -
 - Once you have a working computer, the first step is to restore the gravity
 - assist program (your puzzle input) to the "1202 program alarm" state it had
 - just before the last computer caught fire. To do this, before running the
 - program, replace position 1 with the value 12 and replace position 2 with the
 - value 2. What value is left at position 0 after the program halts?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Control.Monad as C
import qualified Data.Maybe as Maybe

data Instruction
    = Add !Int !Int !Int
    | Mul !Int !Int !Int
    | Halt
  deriving (Show, Eq, Ord)

type Program = IntMap Int

newtype Execution a = Execution (ExecutionState -> (Either ExecutionError a, ExecutionState))

instance Monad Execution where
    (Execution f1) >>= f2 = Execution $ \s0 -> case f1 s0 of
        (Right v, s1) -> let Execution e = f2 v in e s1
        (Left e, s1) -> (Left e, s1)

    return v = Execution $ \s -> (Right v, s)

instance Applicative Execution where
    pure = return
    (<*>) = C.ap

instance Functor Execution where
    fmap f execution = execution >>= \x -> return $ f x

data ExecutionState = ExecutionState
    { exInstructions :: !Program
    , exPc           :: !Int
    } deriving (Show, Eq, Ord)

data ExecutionError = ExecutionError !ErrorReason !Int
  deriving (Show, Eq, Ord)

data ErrorReason = UnknownInstruction | Segfault
  deriving (Show, Eq, Ord)

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: Program -> IO ()
handleInput program = case runProgram program' of
    Right output -> print output
    Left err -> print err
  where
    program' = IntMap.insert 2 2 . IntMap.insert 1 12 $ program

runProgram :: Program -> Either ExecutionError Int
runProgram program = case result of
    Right _ -> Right (Maybe.fromJust . IntMap.lookup 0 . exInstructions $ state)
    Left err -> Left err
  where
    initialState = ExecutionState program 0
    Execution ex = execute
    (result, state) = ex initialState

execute :: Execution ()
execute = do
    instruction <- nextInstruction
    if instruction == Halt
        then return ()
        else do
            executeInstruction instruction
            execute

executeInstruction :: Instruction -> Execution ()
executeInstruction Halt = return ()
executeInstruction (Add src1 src2 destination) = do
    v1 <- readPosition src1
    v2 <- readPosition src2
    writePosition destination (v1 + v2)
executeInstruction (Mul src1 src2 destination) = do
    v1 <- readPosition src1
    v2 <- readPosition src2
    writePosition destination (v1 * v2)

nextInstruction :: Execution Instruction
nextInstruction = do
    opCode <- readCurrent
    case opCode of
        1 -> Add <$> readCurrent <*> readCurrent <*> readCurrent
        2 -> Mul <$> readCurrent <*> readCurrent <*> readCurrent
        99 -> return Halt
        _ -> executionError UnknownInstruction

readCurrent :: Execution Int
readCurrent = do
    pc <- getPc
    current <- readPosition pc
    incPc
    return current

incPc :: Execution ()
incPc = Execution $ \state -> (Right (), state { exPc = exPc state + 1 })

getPc :: Execution Int
getPc = Execution $ \state -> (Right $ exPc state, state)

executionError :: ErrorReason -> Execution a
executionError reason = Execution $ \state ->
    (Left $ ExecutionError reason (exPc state), state)

writePosition :: Int -> Int -> Execution ()
writePosition key value = Execution $ \state ->
    let instructions = exInstructions state
        instructions' = IntMap.insert key value instructions
        state' = state { exInstructions = instructions' }
    in (Right (), state')

readPosition :: Int -> Execution Int
readPosition i = do
    value <- readPositionMay i
    case value of
        Just x -> return x
        Nothing -> executionError Segfault

readPositionMay :: Int -> Execution (Maybe Int)
readPositionMay i = Execution $ \state ->
    (Right (IntMap.lookup i (exInstructions state)), state)

parseInput :: T.Text -> Either P.ParseError Program
parseInput = P.parse (parseProgram <* P.eof) ""

parseProgram :: P.Parsec T.Text () Program
parseProgram = IntMap.fromAscList . zip [0..] <$> (P.int `P.sepBy` comma) <* P.newline
  where
    comma = P.char ','
