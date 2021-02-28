{- All this drifting around in space makes you wonder about the nature of the
 - universe. Does history really repeat itself? You're curious whether the moons
 - will ever return to a previous state.
 -
 - Determine the number of steps that must occur before all of the moons'
 - positions and velocities exactly match a previous point in time.
 -
 - For example, the first example above takes 2772 steps before they exactly
 - match a previous point in time; it eventually returns to the initial state:
 -
 - After 0 steps:
 - pos=<x= -1, y=  0, z=  2>, vel=<x=  0, y=  0, z=  0>
 - pos=<x=  2, y=-10, z= -7>, vel=<x=  0, y=  0, z=  0>
 - pos=<x=  4, y= -8, z=  8>, vel=<x=  0, y=  0, z=  0>
 - pos=<x=  3, y=  5, z= -1>, vel=<x=  0, y=  0, z=  0>
 -
 - After 2770 steps:
 - pos=<x=  2, y= -1, z=  1>, vel=<x= -3, y=  2, z=  2>
 - pos=<x=  3, y= -7, z= -4>, vel=<x=  2, y= -5, z= -6>
 - pos=<x=  1, y= -7, z=  5>, vel=<x=  0, y= -3, z=  6>
 - pos=<x=  2, y=  2, z=  0>, vel=<x=  1, y=  6, z= -2>
 -
 - After 2771 steps:
 - pos=<x= -1, y=  0, z=  2>, vel=<x= -3, y=  1, z=  1>
 - pos=<x=  2, y=-10, z= -7>, vel=<x= -1, y= -3, z= -3>
 - pos=<x=  4, y= -8, z=  8>, vel=<x=  3, y= -1, z=  3>
 - pos=<x=  3, y=  5, z= -1>, vel=<x=  1, y=  3, z= -1>
 -
 - After 2772 steps:
 - pos=<x= -1, y=  0, z=  2>, vel=<x=  0, y=  0, z=  0>
 - pos=<x=  2, y=-10, z= -7>, vel=<x=  0, y=  0, z=  0>
 - pos=<x=  4, y= -8, z=  8>, vel=<x=  0, y=  0, z=  0>
 - pos=<x=  3, y=  5, z= -1>, vel=<x=  0, y=  0, z=  0>
 -
 - Of course, the universe might last for a very long time before repeating.
 - Here's a copy of the second example from above:
 -
 - <x=-8, y=-10, z=0>
 - <x=5, y=5, z=10>
 - <x=2, y=-7, z=3>
 - <x=9, y=-8, z=-3>
 -
 - This set of initial positions takes 4686774924 steps before it repeats a
 - previous state! Clearly, you might need to find a more efficient way to
 - simulate the universe.
 -
 - How many steps does it take to reach the first state that exactly matches a
 - previous state?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

main :: IO ()
main = defaultMain parseInput handleInput

type Position = (Int, Int, Int)
type Velocity = (Int, Int, Int)
data Body = Body !Position !Velocity deriving (Show, Eq, Ord)
type Bodies = [Body]

handleInput :: Bodies -> IO ()
handleInput = print . universeRepeat

universeRepeat :: Bodies -> Int
universeRepeat initial = lcm zRepeat (lcm xRepeat yRepeat)
  where
    simulation = tail $ simulate initial
    xRepeat = (+1) . length . takeWhile (not . sameX initial) $ simulation
    yRepeat = (+1) . length . takeWhile (not . sameY initial) $ simulation
    zRepeat = (+1) . length . takeWhile (not . sameZ initial) $ simulation

sameX :: Bodies -> Bodies -> Bool
sameX bs1 bs2 = xs1 == xs2
  where
    xs1 = concatMap extractX bs1
    xs2 = concatMap extractX bs2
    extractX (Body (x, _, _) (dx, _, _)) = [x, dx]

sameY :: Bodies -> Bodies -> Bool
sameY bs1 bs2 = ys1 == ys2
  where
    ys1 = concatMap extractY bs1
    ys2 = concatMap extractY bs2
    extractY (Body (_, y, _) (_, dy, _)) = [y, dy]

sameZ :: Bodies -> Bodies -> Bool
sameZ bs1 bs2 = zs1 == zs2
  where
    zs1 = concatMap extractZ bs1
    zs2 = concatMap extractZ bs2
    extractZ (Body (_, _, z) (_, _, dz)) = [z, dz]

simulate :: Bodies -> [Bodies]
simulate = iterate step

step :: Bodies -> Bodies
step bodies = map (applyVelocity . updateVelocity bodies) bodies

applyVelocity :: Body -> Body
applyVelocity (Body (x, y, z) (dx, dy, dz)) =
    Body (x + dx, y + dy, z + dz) (dx, dy, dz)

updateVelocity :: Bodies -> Body -> Body
updateVelocity bodies body = foldr updateVelocity' body bodies

updateVelocity' :: Body -> Body -> Body
updateVelocity' (Body (x1, y1, z1) _) (Body (x2, y2, z2) (dx, dy, dz)) =
    Body (x2, y2, z2) (dx', dy', dz')
  where
    dx' = case compare x1 x2 of
        GT -> dx + 1
        LT -> dx - 1
        EQ -> dx
    dy' = case compare y1 y2 of
        GT -> dy + 1
        LT -> dy - 1
        EQ -> dy
    dz' = case compare z1 z2 of
        GT -> dz + 1
        LT -> dz - 1
        EQ -> dz

parseInput :: T.Text -> Either P.ParseError Bodies
parseInput = P.parse (parseBodies <* P.eof) ""

parseBodies :: P.Parsec T.Text () Bodies
parseBodies = parseBody `P.endBy` P.newline

parseBody :: P.Parsec T.Text () Body
parseBody = Body <$> parsePosition <*> pure (0, 0, 0)

parsePosition :: P.Parsec T.Text () Position
parsePosition = (,,)
    <$> (P.string "<x=" *> P.int)
    <*> (P.string ", y=" *> P.int)
    <*> (P.string ", z=" *> P.int <* P.char '>')
