module Day14 where

import AoC.Utils
import Debug.Trace

data Plane = Plane [Row] deriving (Show, Eq)

type Row = [Tile]

data Tile = Empty | Round | Cube deriving (Show, Eq)

data Dir = L | R | U | D deriving (Show, Eq)

input =
  "O....#....\n\
  \O.OO#....#\n\
  \.....##...\n\
  \OO.#O....O\n\
  \.O.....O#.\n\
  \O.#..O.#.#\n\
  \..O..#O..O\n\
  \.......O..\n\
  \#....###..\n\
  \#OO..#...."

parse :: String -> Plane
parse = Plane . (fmap . fmap) pTile . lines
  where
    pTile 'O' = Round
    pTile '#' = Cube
    pTile '.' = Empty

solution :: Plane -> Int
solution = measureLoad . slide U

measureLoad :: Plane -> Int
measureLoad (Plane p) =
  sum $ fmap (\(x, y) -> x * y) $ zip [size, size - 1 ..] (fmap f p)
  where
    f = length . filter (== Round)
    size = length p

slide :: Dir -> Plane -> Plane
slide dir (Plane plane) = case dir of
  U -> Plane $ (iterate update plane) !! 30 -- Maybe find a way to get the fixed-point?

update :: [Row] -> [Row]
update (r1 : r2 : t) = u1 : update (u2 : t)
  where
    (u1, u2) = progress r1 r2
update [l] = [l]

progress :: Row -> Row -> (Row, Row)
progress r1 r2 = unzip $ fmap f (zip r1 r2)
  where
    f (Empty, Round) = (Round, Empty)
    f other = other

main :: IO ()
main = do
  s <- parse <$> readInput
  print $ solution s
