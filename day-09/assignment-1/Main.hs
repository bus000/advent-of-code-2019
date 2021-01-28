{- --- Day 9: Sensor Boost ---
 -
 - You've just said goodbye to the rebooted rover and left Mars when you receive
 - a faint distress signal coming from the asteroid belt. It must be the Ceres
 - monitoring station!
 -
 - In order to lock on to the signal, you'll need to boost your sensors. The
 - Elves send up the latest BOOST program - Basic Operation Of System Test.
 -
 - While BOOST (your puzzle input) is capable of boosting your sensors, for
 - tenuous safety reasons, it refuses to do so until the computer it runs on
 - passes some checks to demonstrate it is a complete Intcode computer.
 -
 - Your existing Intcode computer is missing one key feature: it needs support
 - for parameters in relative mode.
 -
 - Parameters in mode 2, relative mode, behave very similarly to parameters in
 - position mode: the parameter is interpreted as a position. Like position
 - mode, parameters in relative mode can be read from or written to.
 -
 - The important difference is that relative mode parameters don't count from
 - address 0. Instead, they count from a value called the relative base. The
 - relative base starts at 0.
 -
 - The address a relative mode parameter refers to is itself plus the current
 - relative base. When the relative base is 0, relative mode parameters and
 - position mode parameters with the same value refer to the same address.
 -
 - For example, given a relative base of 50, a relative mode parameter of -7
 - refers to memory address 50 + -7 = 43.
 -
 - The relative base is modified with the relative base offset instruction:
 -
 - * Opcode 9 adjusts the relative base by the value of its only parameter. The
 -   relative base increases (or decreases, if the value is negative) by the
 -   value of the parameter.
 -
 - For example, if the relative base is 2000, then after the instruction 109,19,
 - the relative base would be 2019. If the next instruction were 204,-34, then
 - the value at address 1985 would be output.
 -
 - Your Intcode computer will also need a few other capabilities:
 -
 - * The computer's available memory should be much larger than the initial
 -   program. Memory beyond the initial program starts with the value 0 and can
 -   be read or written like any other memory. (It is invalid to try to access
 -   memory at a negative address, though.)
 - * The computer should have support for large numbers. Some instructions near
 -   the beginning of the BOOST program will verify this capability.
 -
 - Here are some example programs that use these features:
 -
 - * 109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99 takes no input
 -   and produces a copy of itself as output.
 - * 1102,34915192,34915192,7,4,7,99,0 should output a 16-digit number.
 - * 104,1125899906842624,99 should output the large number in the middle.
 -
 - The BOOST program will ask for a single input; run it in test mode by
 - providing it the value 1. It will perform a series of checks on each opcode,
 - output any opcodes (and the associated parameter modes) that seem to be
 - functioning incorrectly, and finally output a BOOST keycode.
 -
 - Once your Intcode computer is fully functional, the BOOST program should
 - report no malfunctioning opcodes when run in test mode; it should only output
 - a single value, the BOOST keycode. What BOOST keycode does it produce?
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
import qualified Control.Concurrent.Chan as C
import qualified Control.Exception as C

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

type Execution a = C.ExceptT ExecutionError (C.StateT ExecutionState IO) a

data ExecutionState = ExecutionState
    { exInstructions :: !Program
    , exPc           :: !Int
    , exRelativeBase :: !Int
    , exInput        :: !(C.Chan Int)
    , exOutput       :: !(C.Chan Int)
    }

data ExecutionError = ExecutionError !ErrorReason !ExecutionState

data ErrorReason = UnknownInstruction | InvalidMode | SegFault
  deriving (Show, Eq, Ord)

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: Program -> IO ()
handleInput program = do
    input <- C.newChan
    output <- C.newChan
    let state = ExecutionState program 0 0 input output
    C.writeChan input 1

    execute state
    C.void (C.try $ C.forever $ C.readChan output >>= print
        :: IO (Either C.BlockedIndefinitelyOnMVar ()))
    return ()

execute :: ExecutionState -> IO ()
execute state = C.void $ C.runStateT (C.runExceptT execution) state

execution :: Execution ()
execution = do
    instruction <- nextInstruction
    if instruction == Halt
        then return ()
        else do
            executeInstruction instruction
            execution

executeInstruction :: Instruction -> Execution ()
executeInstruction Halt = return ()
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
    giveOutput output
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

giveOutput :: Int -> Execution ()
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
