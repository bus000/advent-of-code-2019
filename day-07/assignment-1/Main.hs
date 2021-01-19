{- --- Day 7: Amplification Circuit ---
 -
 - Based on the navigational maps, you're going to need to send more power to
 - your ship's thrusters to reach Santa in time. To do this, you'll need to
 - configure a series of amplifiers already installed on the ship.
 -
 - There are five amplifiers connected in series; each one receives an input
 - signal and produces an output signal. They are connected such that the first
 - amplifier's output leads to the second amplifier's input, the second
 - amplifier's output leads to the third amplifier's input, and so on. The first
 - amplifier's input value is 0, and the last amplifier's output leads to your
 - ship's thrusters.
 -
 -        O-------O  O-------O  O-------O  O-------O  O-------O
 -    0 ->| Amp A |->| Amp B |->| Amp C |->| Amp D |->| Amp E |-> (to thrusters)
 -        O-------O  O-------O  O-------O  O-------O  O-------O
 -
 - The Elves have sent you some Amplifier Controller Software (your puzzle
 - input), a program that should run on your existing Intcode computer. Each
 - amplifier will need to run a copy of the program.
 -
 - When a copy of the program starts running on an amplifier, it will first use
 - an input instruction to ask the amplifier for its current phase setting (an
 - integer from 0 to 4). Each phase setting is used exactly once, but the Elves
 - can't remember which amplifier needs which phase setting.
 -
 - The program will then call another input instruction to get the amplifier's
 - input signal, compute the correct output signal, and supply it back to the
 - amplifier with an output instruction. (If the amplifier has not yet received
 - an input signal, it waits until one arrives.)
 -
 - Your job is to find the largest output signal that can be sent to the
 - thrusters by trying every possible combination of phase settings on the
 - amplifiers. Make sure that memory is not shared or reused between copies of
 - the program.
 -
 - For example, suppose you want to try the phase setting sequence 3,1,2,4,0,
 - which would mean setting amplifier A to phase setting 3, amplifier B to
 - setting 1, C to 2, D to 4, and E to 0. Then, you could determine the output
 - signal that gets sent from amplifier E to the thrusters with the following
 - steps:
 -
 - * Start the copy of the amplifier controller software that will run on
 -   amplifier A. At its first input instruction, provide it the amplifier's
 -   phase setting, 3. At its second input instruction, provide it the input
 -   signal, 0. After some calculations, it will use an output instruction to
 -   indicate the amplifier's output signal.
 - * Start the software for amplifier B. Provide it the phase setting (1) and
 -   then whatever output signal was produced from amplifier A. It will then
 -   produce a new output signal destined for amplifier C.
 - * Start the software for amplifier C, provide the phase setting (2) and the
 -   value from amplifier B, then collect its output signal.
 - * Run amplifier D's software, provide the phase setting (4) and input value,
 -   and collect its output signal.
 - * Run amplifier E's software, provide the phase setting (0) and input value,
 -   and collect its output signal.
 -
 - The final output signal from amplifier E would be sent to the thrusters.
 - However, this phase setting sequence may not have been the best one; another
 - sequence might have sent a higher signal to the thrusters.
 -
 - Here are some example programs:
 -
 - * Max thruster signal 43210 (from phase setting sequence 4,3,2,1,0):
 -   3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0
 -
 - * Max thruster signal 54321 (from phase setting sequence 0,1,2,3,4):
 -   3,23,3,24,1002,24,10,24,1002,23,-1,23,
 -   101,5,23,23,1,24,23,23,4,23,99,0,0
 -
 - * Max thruster signal 65210 (from phase setting sequence 1,0,4,3,2):
 -   3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
 -   1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0
 -
 - Try every combination of phase settings on the amplifiers. What is the
 - highest signal that can be sent to the thrusters?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Control.Monad as C
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

newtype Execution a
    = Execution (ExecutionState -> (Either ExecutionError a, ExecutionState))

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
    , exInput        :: ![Int]
    , exOutput       :: ![Int]
    } deriving (Show, Eq, Ord)

data ExecutionError = ExecutionError !ErrorReason !ExecutionState
  deriving (Show, Eq, Ord)

data ErrorReason = UnknownInstruction | Segfault | InvalidMode | NoMoreInput
  deriving (Show, Eq, Ord)

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: Program -> IO ()
handleInput = print . bestPhaseConfig

bestPhaseConfig :: Program -> Int
bestPhaseConfig program = maximum $ do
    phases <- L.permutations [0, 1, 2, 3, 4]
    case runAmplifiers program phases of
        Right output -> return output
        Left (ExecutionError NoMoreInput state) -> exOutput state
        Left _ -> []

runAmplifiers :: Program -> [Int] -> Either ExecutionError Int
runAmplifiers program phases = C.foldM (runAmplifier program) 0 phases

runAmplifier :: Program -> Int -> Int -> Either ExecutionError Int
runAmplifier program input phase = case result of
    Right _ -> Right (head $ exOutput state)
    Left (ExecutionError NoMoreInput _) -> Right (head $ exOutput state)
    Left err -> Left err
  where
    initialState = ExecutionState program 0 [phase, input] []
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
incPc = Execution $ \state -> (Right (), state { exPc = exPc state + 1 })

getPc :: Execution Int
getPc = Execution $ \state -> (Right $ exPc state, state)

executionError :: ErrorReason -> Execution a
executionError reason = Execution $ \state ->
    (Left $ ExecutionError reason state, state)

writePosition :: Int -> Int -> Execution ()
writePosition key value = Execution $ \state ->
    let instructions = exInstructions state
        instructions' = IntMap.insert key value instructions
        state' = state { exInstructions = instructions' }
    in (Right (), state')

readValue :: Value -> Execution Int
readValue (Immediate x) = return x
readValue (Stored x) = readPosition x

readPosition :: Int -> Execution Int
readPosition i = do
    value <- readPositionMay i
    case value of
        Just x -> return x
        Nothing -> executionError Segfault

readPositionMay :: Int -> Execution (Maybe Int)
readPositionMay i = Execution $ \state ->
    (Right (IntMap.lookup i (exInstructions state)), state)

popInput :: Execution Int
popInput = Execution $ \state -> case exInput state of
    (input:inputs) -> (Right input, state { exInput = inputs })
    [] -> (Left $ ExecutionError NoMoreInput state, state)

giveOutput :: Int -> Execution ()
giveOutput output = Execution $ \state ->
    (Right (), state { exOutput = output:exOutput state })

setPc :: Int -> Execution ()
setPc pc = Execution $ \state ->
    (Right (), state { exPc = pc })

parseInput :: T.Text -> Either P.ParseError Program
parseInput = P.parse (parseProgram <* P.eof) ""

parseProgram :: P.Parsec T.Text () Program
parseProgram = IntMap.fromAscList . zip [0..] <$> (P.int `P.sepBy` comma) <* P.newline
  where
    comma = P.char ','
