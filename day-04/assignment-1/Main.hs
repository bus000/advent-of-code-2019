{- --- Day 4: Secure Container ---
 -
 - You arrive at the Venus fuel depot only to discover it's protected by a
 - password. The Elves had written the password on a sticky note, but someone
 - threw it out.
 -
 - However, they do remember a few key facts about the password:
 -
 - * It is a six-digit number.
 - * The value is within the range given in your puzzle input.
 - * Two adjacent digits are the same (like 22 in 122345).
 - * Going from left to right, the digits never decrease; they only ever
 -   increase or stay the same (like 111123 or 135679).
 -
 - Other than the range rule, the following are true:
 -
 - * 111111 meets these criteria (double 11, never decreases).
 - * 223450 does not meet these criteria (decreasing pair of digits 50).
 - * 123789 does not meet these criteria (no double).
 -
 - How many different passwords within the range given in your puzzle input meet
 - these criteria?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Char as C
import qualified Data.List as L

main :: IO ()
main = defaultMain parseInput handleInput

type Bounds = (Int, Int)

handleInput :: Bounds -> IO ()
handleInput (lower, upper)
    = print
    . length
    . filter validPassword
    . map show
    $ [lower..upper]

validPassword :: String -> Bool
validPassword password = correctLength && allNum && repeatingNum && increasing
  where
    correctLength = length password == 6
    allNum = all C.isNumber password
    repeatingNum = (maximum . map length . L.group) password > 1
    increasing = and $ zipWith (<=) password (tail password)

parseInput :: T.Text -> Either P.ParseError Bounds
parseInput = P.parse (parseBounds <* P.eof) ""

parseBounds :: P.Parsec T.Text () Bounds
parseBounds = (,) <$> P.int <* P.char '-' <*> P.int <* P.newline
