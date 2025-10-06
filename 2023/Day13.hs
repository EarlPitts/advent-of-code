module Day13 where

import AoC.Utils
import Data.List
import Data.List.Split (splitOn)

input =
  "#...##..#\n\
  \#....#..#\n\
  \..##..###\n\
  \#####.##.\n\
  \#####.##.\n\
  \..##..###\n\
  \#....#..#"

input' =
  "#.##..##.\n\
  \..#.##.#.\n\
  \##......#\n\
  \##......#\n\
  \..#.##.#.\n\
  \..##..##.\n\
  \#.#.##.#."

findPosition :: [String] -> [Int]
findPosition = go 0 []
  where
    go _ idxs [] = idxs
    go _ idxs [_] = idxs
    go n idxs (x : y : xs) =
      if x == y then go (n + 1) ((n + 1) : idxs) (y : xs) else go (n + 1) idxs (y : xs)

checkMirror :: [String] -> Int -> Bool
checkMirror ls idx = size >= min (length before) (length after)
  where
    (before, after) = splitAt idx ls
    size = length $ takeWhile (uncurry (==)) $ zip (reverse before) after

parse :: String -> [[String]]
parse = fmap lines . splitOn "\n\n"

count :: [String] -> Int
count ls = if null ns then 0 else head ns
  where
    ns = filter (checkMirror ls) $ findPosition ls

solution :: [[String]] -> Int
solution ls = horizontal + vertical
  where
    horizontal = 100 * sum (fmap count ls)
    vertical = sum $ fmap (count . transpose) ls

main = do
  s <- parse <$> readInput
  print $ solution s
