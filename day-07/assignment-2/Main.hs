{- It's no good - in this configuration, the amplifiers can't generate a large
 - enough output signal to produce the thrust you'll need. The Elves quickly
 - talk you through rewiring the amplifiers into a feedback loop:
 -
 -          O-------O  O-------O  O-------O  O-------O  O-------O
 -    0 -+->| Amp A |->| Amp B |->| Amp C |->| Amp D |->| Amp E |-.
 -       |  O-------O  O-------O  O-------O  O-------O  O-------O |
 -       |                                                        |
 -       '--------------------------------------------------------+
 -                                                                |
 -                                                                v
 -                                                         (to thrusters)
 -
 - Most of the amplifiers are connected as they were before; amplifier A's
 - output is connected to amplifier B's input, and so on. However, the output
 - from amplifier E is now connected into amplifier A's input. This creates the
 - feedback loop: the signal will be sent through the amplifiers many times.
 -
 - In feedback loop mode, the amplifiers need totally different phase settings:
 - integers from 5 to 9, again each used exactly once. These settings will cause
 - the Amplifier Controller Software to repeatedly take input and produce output
 - many times before halting. Provide each amplifier its phase setting at its
 - first input instruction; all further input/output instructions are for
 - signals.
 -
 - Don't restart the Amplifier Controller Software on any amplifier during this
 - process. Each one should continue receiving and sending signals until it
 - halts.
 -
 - All signals sent or received in this process will be between pairs of
 - amplifiers except the very first signal and the very last signal. To start
 - the process, a 0 signal is sent to amplifier A's input exactly once.
 -
 - Eventually, the software on the amplifiers will halt after they have
 - processed the final loop. When this happens, the last output signal from
 - amplifier E is sent to the thrusters. Your job is to find the largest output
 - signal that can be sent to the thrusters using the new phase settings and
 - feedback loop arrangement.
 -
 - Here are some example programs:
 -
 - * Max thruster signal 139629729 (from phase setting sequence 9,8,7,6,5):
 -   3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
 -   27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5
 -
 - * Max thruster signal 18216 (from phase setting sequence 9,7,8,5,6):
 -   3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
 -   -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
 -   53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10
 -
 - Try every combination of the new phase settings on the amplifier feedback
 - loop. What is the highest signal that can be sent to the thrusters?
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
import qualified Control.Concurrent as C
import qualified Data.List as L

data Instruction
    = Add !Value !Value !Int
    | Mul !Value !Value !Int
    | Input !Int
    | Output !Value
    | JmpT !Value !Value
    | JmpF !Value !Value
    | Lt !Value !Value !Int
    | Eq !Value !Value !Int
    | Halt
  deriving (Show, Eq, Ord)

data Value = Immediate !Int | Stored !Int deriving (Show, Eq, Ord)

type Program = IntMap Int

type Execution a = C.ExceptT ExecutionError (C.StateT ExecutionState IO) a

data ExecutionState = ExecutionState
    { exInstructions :: !Program
    , exPc           :: !Int
    , exInput        :: !(C.Chan Int)
    , exOutput       :: !(C.Chan Int)
    }

data ExecutionError = ExecutionError !ErrorReason !ExecutionState

data ErrorReason = UnknownInstruction | Segfault | InvalidMode
  deriving (Show, Eq, Ord)

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: Program -> IO ()
handleInput program = bestPhaseConfig program >>= print

bestPhaseConfig :: Program -> IO Int
bestPhaseConfig program = do
    let phases = L.permutations [5, 6, 7, 8, 9]
    outputs <- mapM (runAmplifiers program) phases
    return $! maximum outputs

runAmplifiers :: Program -> [Int] -> IO Int
runAmplifiers _ [] = return 0
runAmplifiers program phases = do
    let n = length phases
    channels <- C.replicateM n C.newChan
    C.void $ C.zipWithM C.writeChan channels phases
    let input = head channels
        channels' = tail channels ++ [input]
        states = zipWith (ExecutionState program 0) channels channels'
    finishedChildren <- C.newChan
    C.void $ C.forM states $ \state -> spawnChild (execute state) finishedChildren

    -- Start programs by supplying 0.
    C.writeChan input 0
    -- Wait for all children to finish.
    C.void $ C.replicateM n (C.readChan finishedChildren)
    C.readChan input

spawnChild :: IO () -> C.Chan () -> IO C.ThreadId
spawnChild io finished = C.forkFinally io (\_ -> C.writeChan finished ())

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

nextInstruction :: Execution Instruction
nextInstruction = do
    current <- readCurrent
    let opCode = current `mod` 100
    m1 <- mode1 current
    m2 <- mode2 current
    case opCode of
        1 -> do
            p1 <- m1 <$> readCurrent
            p2 <- m2 <$> readCurrent
            d <- readCurrent
            return $! Add p1 p2 d
        2 -> do
            p1 <- m1 <$> readCurrent
            p2 <- m2 <$> readCurrent
            d <- readCurrent
            return $! Mul p1 p2 d
        3 -> do
            d <- readCurrent
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
            d <- readCurrent
            return $! Lt p1 p2 d
        8 -> do
            p1 <- m1 <$> readCurrent
            p2 <- m2 <$> readCurrent
            d <- readCurrent
            return $! Eq p1 p2 d
        99 -> return Halt
        _ -> executionError UnknownInstruction
  where
    mode1 x = case x `mod` 1000 `div` 100 of
        0 -> return Stored
        1 -> return Immediate
        _ -> executionError InvalidMode
    mode2 x = case x `mod` 10000 `div` 1000 of
        0 -> return Stored
        1 -> return Immediate
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

writePosition :: Int -> Int -> Execution ()
writePosition key value = C.modify $ \state ->
    let instructions = exInstructions state
        instructions' = IntMap.insert key value instructions
    in state { exInstructions = instructions' }

readValue :: Value -> Execution Int
readValue (Immediate x) = return x
readValue (Stored x) = readPosition x

readPosition :: Int -> Execution Int
readPosition i = do
    instructions <- C.gets exInstructions
    case IntMap.lookup i instructions of
        Just x -> return x
        Nothing -> executionError Segfault

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
