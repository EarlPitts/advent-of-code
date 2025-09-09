module Day09 where

import Utils

input =
  "0 3 6 9 12 15\n\
  \1 3 6 10 15 21\n\
  \10 13 16 21 30 45"

parse :: String -> [[Int]]
parse = fmap (fmap read . words) . lines

diffs :: [Int] -> [Int]
diffs (x : y : ns) = y - x : diffs (y : ns)
diffs [_] = []
diffs [] = []

extrapolate :: [Int] -> Int
extrapolate = sum . fmap last . takeWhile (not . all (== 0)) . iterate diffs

solution :: [[Int]] -> Int
solution = sum . fmap extrapolate

main :: IO ()
main = do
  l <- parse <$> readInput
  print $ solution l
