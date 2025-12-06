module Day06 where

import AoC.Utils
import Control.Monad
import Data.List
import Data.List.Split

data Op = Mult | Add deriving (Show, Eq)

data Problem = Problem {op :: Op, nums :: [Integer]} deriving (Show, Eq)

pOp :: String -> Op
pOp "*" = Mult
pOp "+" = Add

parse :: String -> [Problem]
parse str = fmap f ls
  where
    ls = transpose $ fmap words $ lines str
    f l = Problem (pOp (last l)) (fmap read (init l))

parse' :: String -> [Problem]
parse' str = fmap f (zip chunks ops)
  where
    ls = lines str
    chunks = (fmap . fmap) read $ splitOn [[]] $ fmap join $ fmap words $ transpose numLs
    f (chunk, op) = Problem op chunk
    (numLs, ops) = (init ls, fmap pOp $ words $ (last ls))

result :: Problem -> Integer
result (Problem Add nums) = sum nums
result (Problem Mult nums) = product nums

solution :: [Problem] -> Integer
solution = sum . fmap result

main :: IO ()
main = do
  ps <- parse <$> readInput
  ps' <- parse' <$> readInput
  print $ "Part 1"
  print $ solution ps
  print $ "Part 2"
  print $ solution ps'
