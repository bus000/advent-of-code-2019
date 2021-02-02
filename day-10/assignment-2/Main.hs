{- Once you give them the coordinates, the Elves quickly deploy an Instant
 - Monitoring Station to the location and discover the worst: there are simply
 - too many asteroids.
 -
 - The only solution is complete vaporization by giant laser.
 -
 - Fortunately, in addition to an asteroid scanner, the new monitoring station
 - also comes equipped with a giant rotating laser perfect for vaporizing
 - asteroids. The laser starts by pointing up and always rotates clockwise,
 - vaporizing any asteroid it hits.
 -
 - If multiple asteroids are exactly in line with the station, the laser only
 - has enough power to vaporize one of them before continuing its rotation. In
 - other words, the same asteroids that can be detected can be vaporized, but if
 - vaporizing one asteroid makes another one detectable, the newly-detected
 - asteroid won't be vaporized until the laser has returned to the same position
 - by rotating a full 360 degrees.
 -
 - For example, consider the following map, where the asteroid with the new
 - monitoring station (and laser) is marked X:
 -
 -    .#....#####...#..
 -    ##...##.#####..##
 -    ##...#...#.#####.
 -    ..#.....X...###..
 -    ..#.#.....#....##
 -
 - The first nine asteroids to get vaporized, in order, would be:
 -
 -    .#....###24...#..
 -    ##...##.13#67..9#
 -    ##...#...5.8####.
 -    ..#.....X...###..
 -    ..#.#.....#....##
 -
 - Note that some asteroids (the ones behind the asteroids marked 1, 5, and 7)
 - won't have a chance to be vaporized until the next full rotation. The laser
 - continues rotating; the next nine to be vaporized are:
 -
 -    .#....###.....#..
 -    ##...##...#.....#
 -    ##...#......1234.
 -    ..#.....X...5##..
 -    ..#.9.....8....76
 -
 - The next nine to be vaporized are then:
 -
 -    .8....###.....#..
 -    56...9#...#.....#
 -    34...7...........
 -    ..2.....X....##..
 -    ..1..............
 -
 - Finally, the laser completes its first full rotation (1 through 3), a second
 - rotation (4 through 8), and vaporizes the last asteroid (9) partway through
 - its third rotation:
 -
 -    ......234.....6..
 -    ......1...5.....7
 -    .................
 -    ........X....89..
 -    .................
 -
 - In the large example above (the one with the best monitoring station location
 - at 11,13):
 -
 - * The 1st asteroid to be vaporized is at 11,12.
 - * The 2nd asteroid to be vaporized is at 12,1.
 - * The 3rd asteroid to be vaporized is at 12,2.
 - * The 10th asteroid to be vaporized is at 12,8.
 - * The 20th asteroid to be vaporized is at 16,0.
 - * The 50th asteroid to be vaporized is at 16,9.
 - * The 100th asteroid to be vaporized is at 10,16.
 - * The 199th asteroid to be vaporized is at 9,6.
 - * The 200th asteroid to be vaporized is at 8,2.
 - * The 201st asteroid to be vaporized is at 10,9.
 - * The 299th and final asteroid to be vaporized is at 11,1.
 -
 - The Elves are placing bets on which will be the 200th asteroid to be
 - vaporized. Win the bet by determining which asteroid that will be; what do
 - you get if you multiply its X coordinate by 100 and then add its Y
 - coordinate? (For example, 8,2 becomes 802.)
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Data.Set as Set
import qualified Control.Monad as C
import qualified Data.Function as F
import qualified Data.List as L

main :: IO ()
main = defaultMain parseInput handleInput

type Position = (Int, Int)
type Asteroids = Set.Set Position

handleInput :: Asteroids -> IO ()
handleInput asteroids
    = mapM_ print
    . map (\(x, y) -> x * 100 + y)
    . take 1
    . drop 199
    . eliminationOrder asteroids
    . mostVisible
    $ asteroids

eliminationOrder :: Asteroids -> Position -> [Position]
eliminationOrder withSelf self
    = concat
    . L.transpose
    . L.groupBy (\x y -> direction self x == direction self y)
    . L.sortBy (theAngle <> theDistance)
    $ asteroids
  where
    asteroids = Set.toList $ Set.delete self withSelf
    theAngle = compare `F.on` (angleNorth . direction self)
    theDistance = compare `F.on` distance self

mostVisible :: Asteroids -> Position
mostVisible asteroids = snd . L.maximumBy (compare `F.on` fst) $ do
    asteroid <- Set.toList asteroids
    return (length (directionsWith asteroids asteroid), asteroid)

directionsWith :: Asteroids -> Position -> Set.Set (Int, Int)
directionsWith withSelf self = Set.fromList . map (direction self) $ asteroids
  where
    asteroids = Set.toList $ Set.delete self withSelf

direction :: Position -> Position -> (Int, Int)
direction (x1, y1) (x2, y2) =
    let dx = x2 - x1
        dy = y2 - y1
        gc = gcd dx dy
    in (dx `div` gc, dy `div` gc)

distance :: Position -> Position -> Double
distance (x1, y1) (x2, y2) = sqrt $ ((dx2 - dx1)**2) + ((dy2 - dy1)**2)
  where
    dx1 = fromIntegral x1
    dx2 = fromIntegral x2
    dy1 = fromIntegral y1
    dy2 = fromIntegral y2

angleNorth :: Position -> Double
angleNorth (x, y)
    | x >= 0 = acos (-normalizedY)
    | otherwise = 2*pi - acos (-normalizedY)
  where
    (dx, dy) = (fromIntegral x, fromIntegral y)
    len = sqrt $ dx*dx + dy*dy
    normalizedY = dy / len

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
