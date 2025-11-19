module Day14 where

import AoC.Utils
import Data.List
import Debug.Trace

data Plane = Plane {rows :: [Row]} deriving Eq

type Row = [Tile]

data Tile = Empty | Round | Cube deriving (Show, Eq)

data Dir = L | R | U | D deriving (Show, Eq)

instance Show Plane where
  show (Plane rows) = unlines $ (fmap . fmap) f rows
    where
      f Empty = ' '
      f Round = 'O'
      f Cube = '#'

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

-- solution :: Plane -> Int
solution = take 300 . fmap measureLoad . iterate cyclePlane

cyclePlane :: Plane -> Plane
cyclePlane = slide R . slide D . slide L . slide U

measureLoad :: Plane -> Int
measureLoad (Plane p) =
  sum $ fmap (\(x, y) -> x * y) $ zip [size, size - 1 ..] (fmap f p)
  where
    f = length . filter (== Round)
    size = length p

slide :: Dir -> Plane -> Plane
slide dir plane = case dir of
  U -> (iterate update plane) !! 99 -- Maybe find a way to get the fixed-point?
  D -> south $ (iterate update (south plane)) !! 99
  R -> east' $ (iterate update (east plane)) !! 99
  L -> west $ (iterate update (west plane)) !! 99

update :: Plane -> Plane
update (Plane (r1 : r2 : t)) =
  Plane (u1 : (rows $ update (Plane (u2 : t))))
  where
    (u1, u2) = progress r1 r2
update other = other

south = Plane . reverse . rows

west = Plane . transpose . rows

east = Plane . reverse . transpose . rows

east' = Plane . transpose . reverse . rows

progress :: Row -> Row -> (Row, Row)
progress r1 r2 = unzip $ fmap f (zip r1 r2)
  where
    f (Empty, Round) = (Round, Empty)
    f other = other

main :: IO ()
main = do
  s <- parse <$> readInput
  print $ solution s
