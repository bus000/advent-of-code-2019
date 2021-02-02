{- You now have a complete Intcode computer.
 -
 - Finally, you can lock on to the Ceres distress signal! You just need to boost
 - your sensors using the BOOST program.
 -
 - The program runs in sensor boost mode by providing the input instruction the
 - value 2. Once run, it will boost the sensors automatically, but it might take
 - a few seconds to complete the operation on slower hardware. In sensor boost
 - mode, the program will output a single value: the coordinates of the distress
 - signal.
 -
 - Run the BOOST program in sensor boost mode. What are the coordinates of the
 - distress signal?
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
    C.writeChan input 2

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
