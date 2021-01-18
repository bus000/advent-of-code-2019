{- Now, you just need to figure out how many orbital transfers you (YOU) need to
 - take to get to Santa (SAN).
 -
 - You start at the object YOU are orbiting; your destination is the object SAN
 - is orbiting. An orbital transfer lets you move from any object to an object
 - orbiting or orbited by that object.
 -
 - For example, suppose you have the following map:
 -
 -    COM)B
 -    B)C
 -    C)D
 -    D)E
 -    E)F
 -    B)G
 -    G)H
 -    D)I
 -    E)J
 -    J)K
 -    K)L
 -    K)YOU
 -    I)SAN
 -
 - Visually, the above map of orbits looks like this:
 -
 -                              YOU
 -                             /
 -            G - H       J - K - L
 -           /           /
 -    COM - B - C - D - E - F
 -                   \
 -                    I - SAN
 -
 - In this example, YOU are in orbit around K, and SAN is in orbit around I. To
 - move from K to I, a minimum of 4 orbital transfers are required:
 -
 - * K to J
 - * J to E
 - * E to D
 - * D to I
 -
 - Afterward, the map of orbits looks like this:
 -
 -            G - H       J - K - L
 -           /           /
 -    COM - B - C - D - E - F
 -                   \
 -                    I - SAN
 -                     \
 -                      YOU
 -
 - What is the minimum number of orbital transfers required to move from the
 - object YOU are orbiting to the object SAN is orbiting? (Between the objects
 - they are orbiting - not between YOU and SAN.)
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P

main :: IO ()
main = defaultMain parseInput handleInput

type Orbits = [(String, String)]

data Tree a = Node a [Tree a] deriving (Show, Eq, Ord)

handleInput :: Orbits -> IO ()
handleInput = print . orbitalDistance "SAN" "YOU" . (`buildTree` "COM")

buildTree :: Eq a => [(a, a)] -> a -> Tree a
buildTree connections root = Node root $ map (buildTree connections) children
  where
    children = map snd . filter p $ connections
    p = (root ==) . fst

orbitalDistance :: Eq a => a -> a -> Tree a -> Int
orbitalDistance t1 t2 tree = minimum $ do
    p1 <- pathsTo t1 tree
    p2 <- pathsTo t2 tree
    let (p1', p2') = dropWhileEq p1 p2
    return $! length p1' + length p2'
  where
    dropWhileEq (x:xs) (y:ys)
        | x == y = dropWhileEq xs ys
        | otherwise = (xs, ys)
    dropWhileEq xs ys = (xs, ys)

pathsTo :: Eq a => a -> Tree a -> [[a]]
pathsTo target (Node x children)
    | x == target = [[x]]
    | otherwise = map (x:) . concatMap (pathsTo target) $ children

parseInput :: T.Text -> Either P.ParseError Orbits
parseInput = P.parse (parseOrbits <* P.eof) ""

parseOrbits :: P.Parsec T.Text () Orbits
parseOrbits = parseOrbit `P.endBy` P.newline

parseOrbit :: P.Parsec T.Text () (String, String)
parseOrbit = (,) <$> P.many1 P.alphaNum <* P.char ')' <*> P.many1 P.alphaNum
