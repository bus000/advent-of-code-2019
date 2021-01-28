{- --- Day 10: Monitoring Station ---
 -
 - You fly into the asteroid belt and reach the Ceres monitoring station. The
 - Elves here have an emergency: they're having trouble tracking all of the
 - asteroids and can't be sure they're safe.
 -
 - The Elves would like to build a new monitoring station in a nearby area of
 - space; they hand you a map of all of the asteroids in that region (your
 - puzzle input).
 -
 - The map indicates whether each position is empty (.) or contains an asteroid 
 - (#). The asteroids are much smaller than they appear on the map, and every
 - asteroid is exactly in the center of its marked position. The asteroids can
 - be described with X,Y coordinates where X is the distance from the left edge
 - and Y is the distance from the top edge (so the top-left corner is 0,0 and
 - the position immediately to its right is 1,0).
 -
 - Your job is to figure out which asteroid would be the best place to build a
 - new monitoring station. A monitoring station can detect any asteroid to which
 - it has direct line of sight - that is, there cannot be another asteroid
 - exactly between them. This line of sight can be at any angle, not just lines
 - aligned to the grid or diagonally. The best location is the asteroid that can
 - detect the largest number of other asteroids.
 -
 - For example, consider the following map:
 -
 -    .#..#
 -    .....
 -    #####
 -    ....#
 -    ...##
 -
 - The best location for a new monitoring station on this map is the highlighted
 - asteroid at 3,4 because it can detect 8 asteroids, more than any other
 - location. (The only asteroid it cannot detect is the one at 1,0; its view of
 - this asteroid is blocked by the asteroid at 2,2.) All other asteroids are
 - worse locations; they can detect 7 or fewer other asteroids. Here is the
 - number of other asteroids a monitoring station on each asteroid could detect:
 -
 -    .7..7
 -    .....
 -    67775
 -    ....7
 -    ...87
 -
 - Here is an asteroid (#) and some examples of the ways its line of sight might
 - be blocked. If there were another asteroid at the location of a capital
 - letter, the locations marked with the corresponding lowercase letter would be
 - blocked and could not be detected:
 -
 -    #.........
 -    ...A......
 -    ...B..a...
 -    .EDCG....a
 -    ..F.c.b...
 -    .....c....
 -    ..efd.c.gb
 -    .......c..
 -    ....f...c.
 -    ...e..d..c
 -
 - Here are some larger examples:
 -
 - Best is 5,8 with 33 other asteroids detected:
 -
 -    ......#.#.
 -    #..#.#....
 -    ..#######.
 -    .#.#.###..
 -    .#..#.....
 -    ..#....#.#
 -    #..#....#.
 -    .##.#..###
 -    ##...#..#.
 -    .#....####
 -
 - Best is 1,2 with 35 other asteroids detected:
 -
 -    #.#...#.#.
 -    .###....#.
 -    .#....#...
 -    ##.#.#.#.#
 -    ....#.#.#.
 -    .##..###.#
 -    ..#...##..
 -    ..##....##
 -    ......#...
 -    .####.###.
 -
 - Best is 6,3 with 41 other asteroids detected:
 -
 -    .#..#..###
 -    ####.###.#
 -    ....###.#.
 -    ..###.##.#
 -    ##.##.#.#.
 -    ....###..#
 -    ..#.#..#.#
 -    #..#.#.###
 -    .##...##.#
 -    .....#.#..
 -
 - Best is 11,13 with 210 other asteroids detected:
 -
 -    .#..##.###...#######
 -    ##.############..##.
 -    .#.######.########.#
 -    .###.#######.####.#.
 -    #####.##.#.##.###.##
 -    ..#####..#.#########
 -    ####################
 -    #.####....###.#.#.##
 -    ##.#################
 -    #####.##.###..####..
 -    ..######..##.#######
 -    ####.##.####...##..#
 -    .#####..#.######.###
 -    ##...#.##########...
 -    #.##########.#######
 -    .####.#.###.###.#.##
 -    ....##.##.###..#####
 -    .#.#.###########.###
 -    #.#.#.#####.####.###
 -    ###.##.####.##.#..##
 -
 - Find the best location for a new monitoring station. How many other asteroids
 - can be detected from that location?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Data.Set as Set
import qualified Control.Monad as C

main :: IO ()
main = defaultMain parseInput handleInput

type Position = (Int, Int)
type Asteroids = Set.Set Position

handleInput :: Asteroids -> IO ()
handleInput = print . mostVisible

mostVisible :: Asteroids -> Int
mostVisible asteroids = maximum $ do
    asteroid <- Set.toList asteroids
    return $! length (directionsWith asteroids asteroid)

directionsWith :: Asteroids -> Position -> Set.Set (Int, Int)
directionsWith withSelf self@(x1, y1) = Set.fromList . map f $ asteroids
  where
    asteroids = Set.toList $ Set.delete self withSelf
    f (x2, y2) =
        let dx = x2 - x1
            dy = y2 - y1
            gc = gcd dx dy
        in (dx `div` gc, dy `div` gc)

parseInput :: T.Text -> Either P.ParseError Asteroids
parseInput = P.parse (parseAsteroids <* P.eof) ""

parseAsteroids :: P.Parsec T.Text () Asteroids
parseAsteroids = do
    rows <- P.many (P.oneOf ['.', '#']) `P.endBy` P.newline
    let positions = getAsteroids rows
    return $! Set.fromList positions
  where
    getAsteroids rows = do
       (y, row) <- enumerate rows
       (x, char) <- enumerate row
       C.guard $ char == '#'
       return (x, y)
    enumerate = zip [0..]
