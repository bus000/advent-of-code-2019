{- --- Day 5: Sunny with a Chance of Asteroids ---
 -
 - You're starting to sweat as the ship makes its way toward Mercury. The Elves
 - suggest that you get the air conditioner working by upgrading your ship
 - computer to support the Thermal Environment Supervision Terminal.
 -
 - The Thermal Environment Supervision Terminal (TEST) starts by running a
 - diagnostic program (your puzzle input). The TEST diagnostic program will run
 - on your existing Intcode computer after a few modifications:
 -
 - First, you'll need to add two new instructions:
 -
 - * Opcode 3 takes a single integer as input and saves it to the position given
 -   by its only parameter. For example, the instruction 3,50 would take an
 -   input value and store it at address 50.
 - * Opcode 4 outputs the value of its only parameter. For example, the
 -   instruction 4,50 would output the value at address 50.
 -
 - Programs that use these instructions will come with documentation that
 - explains what should be connected to the input and output. The program 3,0,4,
 - 0,99 outputs whatever it gets as input, then halts.
 -
 - Second, you'll need to add support for parameter modes:
 -
 - Each parameter of an instruction is handled based on its parameter mode.
 - Right now, your ship computer already understands parameter mode 0, position
 - mode, which causes the parameter to be interpreted as a position - if the
 - parameter is 50, its value is the value stored at address 50 in memory. Until
 - now, all parameters have been in position mode.
 -
 - Now, your ship computer will also need to handle parameters in mode 1,
 - immediate mode. In immediate mode, a parameter is interpreted as a value - if
 - the parameter is 50, its value is simply 50.
 -
 - Parameter modes are stored in the same value as the instruction's opcode. The
 - opcode is a two-digit number based only on the ones and tens digit of the
 - value, that is, the opcode is the rightmost two digits of the first value in
 - an instruction. Parameter modes are single digits, one per parameter, read
 - right-to-left from the opcode: the first parameter's mode is in the hundreds
 - digit, the second parameter's mode is in the thousands digit, the third
 - parameter's mode is in the ten-thousands digit, and so on. Any missing modes
 - are 0.
 -
 - For example, consider the program 1002,4,3,4,33.
 -
 - The first instruction, 1002,4,3,4, is a multiply instruction - the rightmost
 - two digits of the first value, 02, indicate opcode 2, multiplication. Then,
 - going right to left, the parameter modes are 0 (hundreds digit), 1 (thousands
 - digit), and 0 (ten-thousands digit, not present and therefore zero):
 -
 -    ABCDE
 -     1002
 -
 -    DE - two-digit opcode,      02 == opcode 2
 -     C - mode of 1st parameter,  0 == position mode
 -     B - mode of 2nd parameter,  1 == immediate mode
 -     A - mode of 3rd parameter,  0 == position mode,
 -                                      omitted due to being a leading zero
 -
 - This instruction multiplies its first two parameters. The first parameter, 4
 - in position mode, works like it did before - its value is the value stored at
 - address 4 (33). The second parameter, 3 in immediate mode, simply has value
 - 3. The result of this operation, 33 * 3 = 99, is written according to the
 - third parameter, 4 in position mode, which also works like it did before - 99
 - is written to address 4.
 -
 - Parameters that an instruction writes to will never be in immediate mode.
 -
 - Finally, some notes:
 -
 - * It is important to remember that the instruction pointer should increase by
 -   the number of values in the instruction after the instruction finishes.
 -   Because of the new instructions, this amount is no longer always 4.
 - * Integers can be negative: 1101,100,-1,4,0 is a valid program (find 100 +
 -   -1, store the result in position 4).
 -
 - The TEST diagnostic program will start by requesting from the user the ID of
 - the system to test by running an input instruction - provide it 1, the ID for
 - the ship's air conditioner unit.
 -
 - It will then perform a series of diagnostic tests confirming that various
 - parts of the Intcode computer, like parameter modes, function correctly. For
 - each test, it will run an output instruction indicating how far the result of
 - the test was from the expected value, where 0 means the test was successful.
 - Non-zero outputs mean that a function is not working correctly; check the
 - instructions that were run before the output instruction to see which one
 - failed.
 -
 - Finally, the program will output a diagnostic code and immediately halt. This
 - final output isn't an error; an output followed immediately by a halt means
 - the program finished. If all outputs were zero except the diagnostic code,
 - the diagnostic program ran successfully.
 -
 - After providing 1 to the only input instruction and passing all the tests,
 - what diagnostic code does the program produce?
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
handleInput program = case runProgram program [1] of
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

parseInput :: T.Text -> Either P.ParseError Program
parseInput = P.parse (parseProgram <* P.eof) ""

parseProgram :: P.Parsec T.Text () Program
parseProgram = IntMap.fromAscList . zip [0..] <$> (P.int `P.sepBy` comma) <* P.newline
  where
    comma = P.char ','
