{- It turns out that this circuit is very timing-sensitive; you actually need to
 - minimize the signal delay.
 -
 - To do this, calculate the number of steps each wire takes to reach each
 - intersection; choose the intersection where the sum of both wires' steps is
 - lowest. If a wire visits a position on the grid multiple times, use the steps
 - value from the first time it visits that position when calculating the total
 - value of a specific intersection.
 -
 - The number of steps a wire takes is the total number of grid squares the wire
 - has entered to get to that location, including the intersection being
 - considered. Again consider the example from above:
 -
 -    ...........
 -    .+-----+...
 -    .|.....|...
 -    .|..+--X-+.
 -    .|..|..|.|.
 -    .|.-X--+.|.
 -    .|..|....|.
 -    .|.......|.
 -    .o-------+.
 -    ...........
 -
 - In the above example, the intersection closest to the central port is reached
 - after 8+5+5+2 = 20 steps by the first wire and 7+6+4+3 = 20 steps by the
 - second wire for a total of 20+20 = 40 steps.
 -
 - However, the top-right intersection is better: the first wire takes only
 - 8+5+2 = 15 and the second wire takes only 7+6+2 = 15, a total of 15+15 = 30
 - steps.
 -
 - Here are the best steps for the extra examples from above:
 -
 - * R75,D30,R83,U83,L12,D49,R71,U7,L72
 -   U62,R66,U55,R34,D71,R55,D58,R83 = 610 steps
 - * R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
 -   U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = 410 steps
 -
 - What is the fewest combined steps the wires must take to reach an
 - intersection?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Map as Map

main :: IO ()
main = defaultMain parseInput handleInput

data Direction = North | East | South | West deriving (Show, Eq, Ord)

data Wire = Wire ![Direction] deriving (Show, Eq, Ord)

handleInput :: (Wire, Wire) -> IO ()
handleInput = print . uncurry closestIntersection

closestIntersection :: Wire -> Wire -> Int
closestIntersection w1 w2 = minimum . filter (/= 0) . Map.elems $ intersections
  where
    trace1 = Map.fromListWith min $ zip (traceWire w1) [0..]
    trace2 = Map.fromListWith min $ zip (traceWire w2) [0..]

    intersections = Map.intersectionWith (+) trace1 trace2

traceWire :: Wire -> [(Int, Int)]
traceWire (Wire directions) = scanl go (0, 0) directions
  where
    go (x, y) North = (x, y + 1)
    go (x, y) East = (x + 1, y)
    go (x, y) South = (x, y - 1)
    go (x, y) West = (x - 1, y)

parseInput :: T.Text -> Either P.ParseError (Wire, Wire)
parseInput = P.parse (parseWires <* P.eof) ""

parseWires :: P.Parsec T.Text () (Wire, Wire)
parseWires = (,) <$> parseWire <* P.newline <*> parseWire <* P.newline

parseWire :: P.Parsec T.Text () Wire
parseWire = Wire . concat <$> parseSegment `P.sepBy` P.char ','

parseSegment :: P.Parsec T.Text () [Direction]
parseSegment = do
    direction <- P.oneOf ['R', 'U', 'L', 'D']
    amount <- P.int
    case direction of
        'R' -> return $! replicate amount East
        'U' -> return $! replicate amount North
        'L' -> return $! replicate amount West
        'D' -> return $! replicate amount South
        _ -> error "Impossible case."
