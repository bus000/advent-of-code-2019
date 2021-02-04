{- --- Day 11: Space Police ---
 -
 - On the way to Jupiter, you're pulled over by the Space Police.
 -
 - "Attention, unmarked spacecraft! You are in violation of Space Law! All
 - spacecraft must have a clearly visible registration identifier! You have 24
 - hours to comply or be sent to Space Jail!"
 -
 - Not wanting to be sent to Space Jail, you radio back to the Elves on Earth
 - for help. Although it takes almost three hours for their reply signal to
 - reach you, they send instructions for how to power up the emergency hull
 - painting robot and even provide a small Intcode program (your puzzle input)
 - that will cause it to paint your ship appropriately.
 -
 - There's just one problem: you don't have an emergency hull painting robot.
 -
 - You'll need to build a new emergency hull painting robot. The robot needs to
 - be able to move around on the grid of square panels on the side of your ship,
 - detect the color of its current panel, and paint its current panel black or
 - white. (All of the panels are currently black.)
 -
 - The Intcode program will serve as the brain of the robot. The program uses
 - input instructions to access the robot's camera: provide 0 if the robot is
 - over a black panel or 1 if the robot is over a white panel. Then, the program
 - will output two values:
 -
 - * First, it will output a value indicating the color to paint the panel the
 -   robot is over: 0 means to paint the panel black, and 1 means to paint the
 -   panel white.
 - * Second, it will output a value indicating the direction the robot should
 -   turn: 0 means it should turn left 90 degrees, and 1 means it should turn
 -   right 90 degrees.
 -
 - After the robot turns, it should always move forward exactly one panel. The
 - robot starts facing up.
 -
 - The robot will continue running for a while like this and halt when it is
 - finished drawing. Do not restart the Intcode computer inside the robot during
 - this process.
 -
 - For example, suppose the robot is about to start running. Drawing black
 - panels as ., white panels as #, and the robot pointing the direction it is
 - facing (< ^ > v), the initial state and region near the robot looks like
 - this:
 -
 -    .....
 -    .....
 -    ..^..
 -    .....
 -    .....
 -
 - The panel under the robot (not visible here because a ^ is shown instead) is
 - also black, and so any input instructions at this point should be provided 0.
 - Suppose the robot eventually outputs 1 (paint white) and then 0 (turn left).
 - After taking these actions and moving forward one panel, the region now looks
 - like this:
 -
 -    .....
 -    .....
 -    .<#..
 -    .....
 -    .....
 -
 - Input instructions should still be provided 0. Next, the robot might output 0
 - (paint black) and then 0 (turn left):
 -
 -    .....
 -    .....
 -    ..#..
 -    .v...
 -    .....
 -
 - After more outputs (1,0, 1,0):
 -
 -    .....
 -    .....
 -    ..^..
 -    .##..
 -    .....
 -
 - The robot is now back where it started, but because it is now on a white
 - panel, input instructions should be provided 1. After several more outputs
 - (0,1, 1,0, 1,0), the area looks like this:
 -
 -    .....
 -    ..<#.
 -    ...#.
 -    .##..
 -    .....
 -
 - Before you deploy the robot, you should probably have an estimate of the area
 - it will cover: specifically, you need to know the number of panels it paints
 - at least once, regardless of color. In the example above, the robot painted 6
 - panels at least once. (It painted its starting panel twice, but that panel is
 - still only counted once; it also never painted the panel it ended on.)
 -
 - Build a new emergency hull painting robot and run the Intcode program on it.
 - How many panels does it paint at least once?
 -}
module Main where

import AdventOfCode
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

data Instruction
    = Add !Value !Value !Value
    | Mul !Value !Value !Value
    | Input !Value
    | Output !Value
    | JmpT !Value !Value
    | JmpF !Value !Value
    | Lt !Value !Value !Value
    | Eq !Value !Value !Value
    | AdjustRelativeBase !Value
    | Halt
  deriving (Show, Eq, Ord)

data Value = Immediate !Int | Stored !Int | Relative !Int
    deriving (Show, Eq, Ord)

type Program = IntMap Int

data ProgramOutput = Halted | ProgramOutput !Int

type Execution a = C.ExceptT ExecutionError (C.StateT ExecutionState IO) a

data ExecutionState = ExecutionState
    { exInstructions :: !Program
    , exPc           :: !Int
    , exRelativeBase :: !Int
    , exInput        :: !(C.Chan Int)
    , exOutput       :: !(C.Chan ProgramOutput)
    }

data ExecutionError = ExecutionError !ErrorReason !ExecutionState

data ErrorReason = UnknownInstruction | InvalidMode | SegFault
  deriving (Show, Eq, Ord)

data Turn = TLeft | TRight
    deriving (Show, Eq, Ord)

data Direction = North | East | South | West
    deriving (Show, Eq, Ord)

data Robot = Robot !Direction !(Int, Int) !(Set.Set (Int, Int))
    deriving (Show, Eq, Ord)

type Hull = Set.Set (Int, Int)

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: Program -> IO ()
handleInput program = do
    input <- C.newChan
    output <- C.newChan
    let state = ExecutionState program 0 0 input output
        hull = Set.empty
        robot = Robot North (0, 0) Set.empty

    C.void $ C.forkIO (execute state)
    visited <- runRobot hull robot input output
    print $! Set.size visited

runRobot :: Hull -> Robot -> C.Chan Int -> C.Chan ProgramOutput -> IO (Set.Set (Int, Int))
runRobot hull (Robot direction position visited) input output = do
    let color = if position `Set.member` hull then 1 else 0
    C.writeChan input color
    paint <- C.readChan output

    case paint of
        Halted -> return visited
        ProgramOutput p -> do
            ProgramOutput cd <- C.readChan output
            let hull' = if p == 0
                    then Set.delete position hull
                    else Set.insert position hull
                direction' = turn direction (if cd == 0 then TLeft else TRight)
                position' = move position direction'
                visited' = Set.insert position' visited
                robot' = Robot direction' position' visited'
            runRobot hull' robot' input output

turn :: Direction -> Turn -> Direction
turn North TLeft = West
turn North TRight = East
turn East TLeft = North
turn East TRight = South
turn South TLeft = East
turn South TRight = West
turn West TLeft = South
turn West TRight = North

move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) North = (x, y + 1)
move (x, y) East = (x + 1, y)
move (x, y) South = (x, y - 1)
move (x, y) West = (x - 1, y)

execute :: ExecutionState -> IO ()
execute state = C.void $ C.runStateT (C.runExceptT execution) state

execution :: Execution ()
execution = do
    instruction <- nextInstruction
    executeInstruction instruction
    C.when (instruction /= Halt) execution

executeInstruction :: Instruction -> Execution ()
executeInstruction Halt = giveOutput Halted
executeInstruction (Add src1 src2 destination) = do
    v1 <- readValue src1
    v2 <- readValue src2
    writePosition destination (v1 + v2)
executeInstruction (Mul src1 src2 destination) = do
    v1 <- readValue src1
    v2 <- readValue src2
    writePosition destination (v1 * v2)
executeInstruction (Input destination) = do
    input <- popInput
    writePosition destination input
executeInstruction (Output src) = do
    output <- readValue src
    giveOutput $! ProgramOutput output
executeInstruction (JmpT src1 src2) = do
    predicate <- readValue src1
    position <- readValue src2
    C.when (predicate /= 0) $ setPc position
executeInstruction (JmpF src1 src2) = do
    predicate <- readValue src1
    position <- readValue src2
    C.when (predicate == 0) $ setPc position
executeInstruction (Lt src1 src2 destination) = do
    v1 <- readValue src1
    v2 <- readValue src2
    if v1 < v2
        then writePosition destination 1
        else writePosition destination 0
executeInstruction (Eq src1 src2 destination) = do
    v1 <- readValue src1
    v2 <- readValue src2
    if v1 == v2
        then writePosition destination 1
        else writePosition destination 0
executeInstruction (AdjustRelativeBase src) = do
    v <- readValue src
    C.modify $ \state -> state { exRelativeBase = exRelativeBase state + v }

nextInstruction :: Execution Instruction
nextInstruction = do
    current <- readCurrent
    let opCode = current `mod` 100
    m1 <- mode1 current
    m2 <- mode2 current
    m3 <- mode3 current
    case opCode of
        1 -> do
            p1 <- m1 <$> readCurrent
            p2 <- m2 <$> readCurrent
            d <- m3 <$> readCurrent
            return $! Add p1 p2 d
        2 -> do
            p1 <- m1 <$> readCurrent
            p2 <- m2 <$> readCurrent
            d <- m3 <$> readCurrent
            return $! Mul p1 p2 d
        3 -> do
            d <- m1 <$> readCurrent
            return $! Input d
        4 -> do
            s <- m1 <$> readCurrent
            return $! Output s
        5 -> do
            predicate <- m1 <$> readCurrent
            position <- m2 <$> readCurrent
            return $! JmpT predicate position
        6 -> do
            predicate <- m1 <$> readCurrent
            position <- m2 <$> readCurrent
            return $! JmpF predicate position
        7 -> do
            p1 <- m1 <$> readCurrent
            p2 <- m2 <$> readCurrent
            d <- m3 <$> readCurrent
            return $! Lt p1 p2 d
        8 -> do
            p1 <- m1 <$> readCurrent
            p2 <- m2 <$> readCurrent
            d <- m3 <$> readCurrent
            return $! Eq p1 p2 d
        9 -> do
            p1 <- m1 <$> readCurrent
            return $! AdjustRelativeBase p1
        99 -> return Halt
        _ -> executionError UnknownInstruction
  where
    mode1 x = case x `mod` 1000 `div` 100 of
        0 -> return Stored
        1 -> return Immediate
        2 -> return Relative
        _ -> executionError InvalidMode
    mode2 x = case x `mod` 10000 `div` 1000 of
        0 -> return Stored
        1 -> return Immediate
        2 -> return Relative
        _ -> executionError InvalidMode
    mode3 x = case x `mod` 100000 `div` 10000 of
        0 -> return Stored
        1 -> return Immediate
        2 -> return Relative
        _ -> executionError InvalidMode

readCurrent :: Execution Int
readCurrent = do
    pc <- getPc
    current <- readPosition pc
    incPc
    return current

incPc :: Execution ()
incPc = C.modify $ \state -> state { exPc = exPc state + 1 }

getPc :: Execution Int
getPc = C.gets exPc

executionError :: ErrorReason -> Execution a
executionError reason = do
    state <- C.get
    C.throwError $! ExecutionError reason state

writePosition :: Value -> Int -> Execution ()
writePosition key value = do
    i <- case key of
        (Immediate x) -> return x
        (Stored x) -> return x
        (Relative x) -> (+x) <$> C.gets exRelativeBase
    if i < 0
        then executionError SegFault
        else C.modify $ \state ->
            let instructions = exInstructions state
                instructions' = IntMap.insert i value instructions
            in state { exInstructions = instructions' }

readValue :: Value -> Execution Int
readValue (Immediate x) = return x
readValue (Stored x) = readPosition x
readValue (Relative x) = do
    rel <- C.gets exRelativeBase
    readPosition (rel + x)

readPosition :: Int -> Execution Int
readPosition i
    | i < 0 = executionError SegFault
    | otherwise = do
        instructions <- C.gets exInstructions
        case IntMap.lookup i instructions of
            Just x -> return x
            Nothing -> return 0

popInput :: Execution Int
popInput = do
    inputChan <- C.gets exInput
    C.liftIO $ C.readChan inputChan

giveOutput :: ProgramOutput -> Execution ()
giveOutput output = do
    outputChan <- C.gets exOutput
    C.liftIO $ C.writeChan outputChan output

setPc :: Int -> Execution ()
setPc pc = C.modify $ \state -> state { exPc = pc }

parseInput :: T.Text -> Either P.ParseError Program
parseInput = P.parse (parseProgram <* P.eof) ""

parseProgram :: P.Parsec T.Text () Program
parseProgram = IntMap.fromAscList . zip [0..] <$> (P.int `P.sepBy` comma) <* P.newline
  where
    comma = P.char ','
