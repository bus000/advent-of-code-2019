{- --- Day 3: Crossed Wires ---
 -
 - The gravity assist was successful, and you're well on your way to the Venus
 - refuelling station. During the rush back on Earth, the fuel management system
 - wasn't completely installed, so that's next on the priority list.
 -
 - Opening the front panel reveals a jumble of wires. Specifically, two wires
 - are connected to a central port and extend outward on a grid. You trace the
 - path each wire takes as it leaves the central port, one wire per line of text
 - (your puzzle input).
 -
 - The wires twist and turn, but the two wires occasionally cross paths. To fix
 - the circuit, you need to find the intersection point closest to the central
 - port. Because the wires are on a grid, use the Manhattan distance for this
 - measurement. While the wires do technically cross right at the central port
 - where they both start, this point does not count, nor does a wire count as
 - crossing with itself.
 -
 - For example, if the first wire's path is R8,U5,L5,D3, then starting from the
 - central port (o), it goes right 8, up 5, left 5, and finally down 3:
 -
 -    ...........
 -    ...........
 -    ...........
 -    ....+----+.
 -    ....|....|.
 -    ....|....|.
 -    ....|....|.
 -    .........|.
 -    .o-------+.
 -    ...........
 -
 - Then, if the second wire's path is U7,R6,D4,L4, it goes up 7, right 6, down
 - 4, and left 4:
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
 - These wires cross at two locations (marked X), but the lower-left one is
 - closer to the central port: its distance is 3 + 3 = 6.
 -
 - Here are a few more examples:
 -
 - * R75,D30,R83,U83,L12,D49,R71,U7,L72
 -   U62,R66,U55,R34,D71,R55,D58,R83 = distance 159
 - * R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
 -   U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = distance 135
 -
 - What is the Manhattan distance from the central port to the closest
 - intersection?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Set as Set

main :: IO ()
main = defaultMain parseInput handleInput

data Direction = North | East | South | West deriving (Show, Eq, Ord)

data Wire = Wire ![Direction] deriving (Show, Eq, Ord)

handleInput :: (Wire, Wire) -> IO ()
handleInput (w1, w2)
    = print
    . Set.findMin
    . Set.delete 0
    . Set.map (uncurry manhattan)
    $ Set.intersection trace1 trace2
  where
    trace1 = Set.fromList $ traceWire w1
    trace2 = Set.fromList $ traceWire w2

traceWire :: Wire -> [(Int, Int)]
traceWire (Wire directions) = scanl go (0, 0) directions
  where
    go (x, y) North = (x, y + 1)
    go (x, y) East = (x + 1, y)
    go (x, y) South = (x, y - 1)
    go (x, y) West = (x - 1, y)

manhattan :: Int -> Int -> Int
manhattan x y = abs x + abs y

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
