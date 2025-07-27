module Utils where

readInput = readFile "input.txt"

readInputList = words <$> readFile "input.txt"

solveList :: ([String] -> Int) -> IO Int
solveList solution = solution <$> readInputList
