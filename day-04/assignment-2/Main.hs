{- An Elf just remembered one more important detail: the two adjacent matching
 - digits are not part of a larger group of matching digits.
 -
 - Given this additional criterion, but still ignoring the range rule, the
 - following are now true:
 -
 - * 112233 meets these criteria because the digits never decrease and all
 -   repeated digits are exactly two digits long.
 - * 123444 no longer meets the criteria (the repeated 44 is part of a larger
 -   group of 444).
 - * 111122 meets the criteria (even though 1 is repeated more than twice, it
 -   still contains a double 22).
 -
 - How many different passwords within the range given in your puzzle input meet
 - all of the criteria?
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
    repeatingNum = 2 `elem` (map length . L.group $ password)
    increasing = and $ zipWith (<=) password (tail password)

parseInput :: T.Text -> Either P.ParseError Bounds
parseInput = P.parse (parseBounds <* P.eof) ""

parseBounds :: P.Parsec T.Text () Bounds
parseBounds = (,) <$> P.int <* P.char '-' <*> P.int <* P.newline
