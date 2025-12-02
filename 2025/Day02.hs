module Day02 where

import AoC.Utils
import Data.List
import Data.List.Split

parse :: String -> [Int]
parse = concat . fmap f . splitOn ","
  where
    f str = case splitOn "-" str of
      [from, to] -> [(read from) .. (read to)]

invalid :: String -> Bool
invalid str = fstHalf == sndHalf
  where
    fstHalf = take (div len 2) str
    sndHalf = drop (div len 2) str
    len = length str

invalid' :: String -> Bool
invalid' str = any f $ chunksOf <$> [1 .. 5] <*> [str]
  where
    f l = length l > 1 && (1 == (length $ groupBy (==) l))

solution :: [Int] -> Int
solution = sum . fmap read . filter invalid . fmap show

solution' :: [Int] -> Int
solution' = sum . fmap read . filter invalid' . fmap show

main = do
  ranges <- parse <$> readInput
  putStrLn "Part 1"
  print $ solution ranges
  putStrLn "Part 2"
  print $ solution' ranges
