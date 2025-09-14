module Day11 where

import Day09 (diffs)
import Control.Comonad
import Control.Monad
import Data.List
import Grid
import Utils

input =
  "...#......\n\
  \.......#..\n\
  \#.........\n\
  \..........\n\
  \......#...\n\
  \.#........\n\
  \.........#\n\
  \..........\n\
  \.......#..\n\
  \#...#....."

data Tile = Galaxy | Empty deriving (Show, Eq)

type Space = [[Tile]]

type Pos = (Int, Int)

parse :: String -> Space
parse = (fmap . fmap) parseChar . lines
  where
    parseChar '.' = Empty
    parseChar '#' = Galaxy

expand :: Space -> Space
expand = expandCols . expandRows
  where
    expandRows = foldr f []
    expandCols = transpose . expandRows . transpose
    f r rs = if all (== Empty) r then r : r : rs else r : rs

expandN :: Int -> Space -> Space
expandN n = expandCols . expandRows
  where
    expandRows = foldr f []
    expandCols = transpose . expandRows . transpose
    f r rs = if all (== Empty) r then replicate n r <> rs else r : rs

galaxies :: Grid Tile -> [Pos]
galaxies g = fmap snd $ filter ((/= Empty) . fst) $ join $ toLists $ f <<= g
  where
    f g = (focus g, getPos g)

solution :: Space -> Int
solution =
  (`div` 2)
    . sum
    . distances
    . galaxies
    . fromLists
    . expand

distance :: Pos -> Pos -> Int
distance (x, y) (x', y') = abs (x - x') + abs (y - y')

distances :: [Pos] -> [Int]
distances ps = do
  g <- ps
  g' <- filter (/= g) ps
  return $ distance g g'

solution' :: (Space -> Space) -> Space -> Int
solution' expand =
  (`div` 2)
    . sum
    . distances
    . galaxies
    . fromLists
    . expand

main = do
  -- let space = parse input
  space <- parse <$> readInput
  -- print $ fromLists $ expand space
  -- print $ solution (expand 2) space
  let [diff] = diffs $ take 2 $ flip solution' space . expandN <$> [2 ..]
  print $ solution space + ((1000000 - 2) * diff)
