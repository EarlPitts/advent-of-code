module Day15 where

import Control.Monad.State
import Data.Char
import Data.List.Split
import AoC.Utils

input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

hash :: String -> Int
hash = foldl hashSingle 0

hashSingle :: Int -> Char -> Int
hashSingle curr c = mod ((curr + ord c) * 17) 256

solution :: String -> Int
solution = sum . (fmap hash) . splitOn ","

main = do
  input <- readInput
  print $ solution input
