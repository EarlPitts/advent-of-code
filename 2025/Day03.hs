module Day03 where

import AoC.Utils
import Data.List

parse :: String -> [[Int]]
parse = ((fmap . fmap) (read . singleton)) . lines

solution :: Int -> [[Int]] -> Int
solution n = sum . fmap read . fmap (mkStr . turnOnN n)
  where
    mkStr = concat . fmap show

turnOnN :: Int -> [Int] -> [Int]
turnOnN len bank = go len bank []
  where
    go 1 b acc = reverse $ maximum b : acc
    go n b acc =
      let biggest = maximum $ take ((length b) - (n - 1)) b
          Just idx = elemIndex biggest b
       in go (n - 1) (drop (idx + 1) b) (biggest : acc)

main :: IO ()
main = do
  banks <- parse <$> readInput
  print $ solution 2 banks
  print $ solution 12 banks
