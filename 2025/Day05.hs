module Day06 where

import AoC.Utils
import Data.List
import Data.List.Split

data Range = Range {bottom :: Integer, top :: Integer} deriving (Show, Eq)

instance Ord Range where
  (<=) (Range b _) (Range b' _) = b <= b'

parse :: String -> ([Range], [Integer])
parse str = (rs, is)
  where
    [rsStr, isStr] = splitOn "\n\n" str
    is = fmap read $ lines isStr
    rs = fmap f $ lines rsStr
    f line = case splitOn "-" line of
      [x, y] -> Range (read x) (read y)

solution :: [Range] -> [Integer] -> Int
solution rs is = length $ filter f is
  where
    f i = any (includes i) rs

includes :: Integer -> Range -> Bool
includes n (Range b t) = n >= b && n <= t

overlaps :: Range -> Range -> Bool
overlaps (Range b t) (Range b' t') =
  (b' >= b && b' <= t) || (t' >= b && t' <= t)

merge :: Range -> Range -> Range
merge (Range b t) (Range b' t') =
  Range (min b b') (max t t')

solution' :: [Range] -> Integer
solution' = sum . fmap count . fixpoint . iterate mergeRanges . sort

count :: Range -> Integer
count (Range b t) = t - b + 1

mergeRanges :: [Range] -> [Range]
mergeRanges = go
  where
    go [] = []
    go [r] = [r]
    go (r1 : r2 : rs) =
      if overlaps r1 r2
        then merge r1 r2 : go rs
        else r1 : go (r2 : rs)

main :: IO ()
main = do
  (rs, is) <- parse <$> readInput
  print $ "Part 1"
  print $ solution rs is
  print $ "Part 2"
  print $ solution' rs
