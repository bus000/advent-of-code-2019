{- "Good, the new computer seems to be working correctly! Keep it nearby during
 - this mission - you'll probably use it again. Real Intcode computers support
 - many more features than your new one, but we'll let you know what they are as
 - you need them."
 -
 - "However, your current priority should be to complete your gravity assist
 - around the Moon. For this mission to succeed, we should settle on some
 - terminology for the parts you've already built."
 -
 - Intcode programs are given as a list of integers; these values are used as
 - the initial state for the computer's memory. When you run an Intcode program,
 - make sure to start by initializing memory to the program's values. A position
 - in memory is called an address (for example, the first value in memory is at
 - "address 0").
 -
 - Opcodes (like 1, 2, or 99) mark the beginning of an instruction. The values
 - used immediately after an opcode, if any, are called the instruction's
 - parameters. For example, in the instruction 1,2,3,4, 1 is the opcode; 2, 3,
 - and 4 are the parameters. The instruction 99 contains only an opcode and has
 - no parameters.
 -
 - The address of the current instruction is called the instruction pointer; it
 - starts at 0. After an instruction finishes, the instruction pointer increases
 - by the number of values in the instruction; until you add more instructions
 - to the computer, this is always 4 (1 opcode + 3 parameters) for the add and
 - multiply instructions. (The halt instruction would increase the instruction
 - pointer by 1, but it halts the program instead.)
 -
 - "With terminology out of the way, we're ready to proceed. To complete the
 - gravity assist, you need to determine what pair of inputs produces the output
 - 19690720."
 -
 - The inputs should still be provided to the program by replacing the values at
 - addresses 1 and 2, just like before. In this program, the value placed in
 - address 1 is called the noun, and the value placed in address 2 is called the
 - verb. Each of the two input values will be between 0 and 99, inclusive.
 -
 - Once the program has halted, its output is available at address 0, also just
 - like before. Each time you try a pair of inputs, make sure you first reset
 - the computer's memory to the values in the program (your puzzle input) - in
 - other words, don't reuse memory from a previous attempt.
 -
 - Find the input noun and verb that cause the program to produce the output
 - 19690720. What is 100 * noun + verb? (For example, if noun=12 and verb=2, the
 - answer would be 1202.)
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
handleInput program = print $ noun*100 + verb
  where
    (noun, verb) = findArguments program

findArguments :: Program -> (Int, Int)
findArguments program = head . filter p $ arguments
  where
    p = uncurry (testArguments program)
    arguments = (,) <$> [0..99] <*> [0..99]

testArguments :: Program -> Int -> Int -> Bool
testArguments program x y = case runProgram program' of
    Right output -> output == 19690720
    Left _ -> False
  where
    program' = IntMap.insert 1 x . IntMap.insert 2 y $ program

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
