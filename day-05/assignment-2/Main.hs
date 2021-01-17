{- The air conditioner comes online! Its cold air feels good for a while, but
 - then the TEST alarms start to go off. Since the air conditioner can't vent
 - its heat anywhere but back into the spacecraft, it's actually making the air
 - inside the ship warmer.
 -
 - Instead, you'll need to use the TEST to extend the thermal radiators.
 - Fortunately, the diagnostic program (your puzzle input) is already equipped
 - for this. Unfortunately, your Intcode computer is not.
 -
 - Your computer is only missing a few opcodes:
 -
 - * Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the
 -   instruction pointer to the value from the second parameter. Otherwise, it
 -   does nothing.
 - * Opcode 6 is jump-if-false: if the first parameter is zero, it sets the
 -   instruction pointer to the value from the second parameter. Otherwise, it
 -   does nothing.
 - * Opcode 7 is less than: if the first parameter is less than the second
 -   parameter, it stores 1 in the position given by the third parameter.
 -   Otherwise, it stores 0.
 - * Opcode 8 is equals: if the first parameter is equal to the second
 -   parameter, it stores 1 in the position given by the third parameter.
 -   Otherwise, it stores 0.
 -
 - Like all instructions, these instructions need to support parameter modes as
 - described above.
 -
 - Normally, after an instruction is finished, the instruction pointer increases
 - by the number of values in that instruction. However, if the instruction
 - modifies the instruction pointer, that value is used and the instruction
 - pointer is not automatically increased.
 -
 - For example, here are several programs that take one input, compare it to the
 - value 8, and then produce one output:
 -
 - * 3,9,8,9,10,9,4,9,99,-1,8 - Using position mode, consider whether the input
 -   is equal to 8; output 1 (if it is) or 0 (if it is not).
 - * 3,9,7,9,10,9,4,9,99,-1,8 - Using position mode, consider whether the input
 -   is less than 8; output 1 (if it is) or 0 (if it is not).
 - * 3,3,1108,-1,8,3,4,3,99 - Using immediate mode, consider whether the input
 -   is equal to 8; output 1 (if it is) or 0 (if it is not).
 - * 3,3,1107,-1,8,3,4,3,99 - Using immediate mode, consider whether the input
 -   is less than 8; output 1 (if it is) or 0 (if it is not).
 -
 - Here are some jump tests that take an input, then output 0 if the input was
 - zero or 1 if the input was non-zero:
 -
 - * 3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9 (using position mode)
 - * 3,3,1105,-1,9,1101,0,0,12,4,12,99,1 (using immediate mode)
 -
 - Here's a larger example:
 -
 -    3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
 -    1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
 -    999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
 -
 - The above example program uses an input instruction to ask for a single
 - number. The program will then output 999 if the input value is below 8,
 - output 1000 if the input value is equal to 8, or output 1001 if the input
 - value is greater than 8.
 -
 - This time, when the TEST diagnostic program runs its input instruction to get
 - the ID of the system to test, provide it 5, the ID for the ship's thermal
 - radiator controller. This diagnostic test suite only outputs one number, the
 - diagnostic code.
 -
 - What is the diagnostic code for system ID 5?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Control.Monad as C

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

data ExecutionError = ExecutionError !ErrorReason !Int
  deriving (Show, Eq, Ord)

data ErrorReason = UnknownInstruction | Segfault | InvalidMode | NoMoreInput
  deriving (Show, Eq, Ord)

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: Program -> IO ()
handleInput program = case runProgram program [5] of
    Right (diagnostic:tests) -> if sum tests == 0
        then print diagnostic
        else putStrLn . concat $ ["Test error: ", show tests]
    Right [] -> putStrLn "Error program gave no output"
    Left err -> putStrLn . concat $ ["Execution error: ", show err]

runProgram :: Program -> [Int] -> Either ExecutionError [Int]
runProgram program input = case result of
    Right _ -> Right (exOutput state)
    Left err -> Left err
  where
    initialState = ExecutionState program 0 input []
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
    (Left $ ExecutionError reason (exPc state), state)

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
    [] -> (Left $ ExecutionError NoMoreInput (exPc state), state)

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
