{- --- Day 8: Space Image Format ---
 -
 - The Elves' spirits are lifted when they realize you have an opportunity to
 - reboot one of their Mars rovers, and so they are curious if you would spend a
 - brief sojourn on Mars. You land your ship near the rover.
 -
 - When you reach the rover, you discover that it's already in the process of
 - rebooting! It's just waiting for someone to enter a BIOS password. The Elf
 - responsible for the rover takes a picture of the password (your puzzle input)
 - and sends it to you via the Digital Sending Network.
 -
 - Unfortunately, images sent via the Digital Sending Network aren't encoded
 - with any normal encoding; instead, they're encoded in a special Space Image
 - Format. None of the Elves seem to remember why this is the case. They send
 - you the instructions to decode it.
 -
 - Images are sent as a series of digits that each represent the color of a
 - single pixel. The digits fill each row of the image left-to-right, then move
 - downward to the next row, filling rows top-to-bottom until every pixel of the
 - image is filled.
 -
 - Each image actually consists of a series of identically-sized layers that are
 - filled in this way. So, the first digit corresponds to the top-left pixel of
 - the first layer, the second digit corresponds to the pixel to the right of
 - that on the same layer, and so on until the last digit, which corresponds to
 - the bottom-right pixel of the last layer.
 -
 - For example, given an image 3 pixels wide and 2 pixels tall, the image data
 - 123456789012 corresponds to the following image layers:
 -
 -    Layer 1: 123
 -             456
 -
 -    Layer 2: 789
 -             012
 -
 - The image you received is 25 pixels wide and 6 pixels tall.
 -
 - To make sure the image wasn't corrupted during transmission, the Elves would
 - like you to find the layer that contains the fewest 0 digits. On that layer,
 - what is the number of 1 digits multiplied by the number of 2 digits?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Char as Char
import qualified Data.Function as F

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: [Int] -> IO ()
handleInput = print . solve

solve :: [Int] -> Int
solve input = length ones * length twos
  where
    layers = L.chunksOf (25 * 6) input
    layer = L.minimumBy (compare `F.on` filter (== 0)) layers
    ones = filter (== 1) layer
    twos = filter (== 2) layer

parseInput :: T.Text -> Either P.ParseError [Int]
parseInput = P.parse (parseInts <* P.eof) ""

parseInts :: P.Parsec T.Text () [Int]
parseInts = map Char.digitToInt <$> P.many P.digit <* P.newline
