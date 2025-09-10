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
extrapolate =
  sum
    . fmap last
    . takeWhile (not . all (== 0))
    . iterate diffs

signs :: [Int] -> [Int]
signs (x : y : xs) = x : -y : signs xs
signs [x] = [x]
signs [] = []

extrapolateBack :: [Int] -> Int
extrapolateBack =
  sum
    . signs
    . fmap head
    . takeWhile (not . all (== 0))
    . iterate diffs

solution :: [[Int]] -> Int
solution = sum . fmap extrapolate

solution' :: [[Int]] -> Int
solution' = sum . fmap extrapolateBack

main :: IO ()
main = do
  l <- parse <$> readInput
  print $ solution l
  print $ solution' l
