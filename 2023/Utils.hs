module Utils where

import Text.Parsec
import Text.Parsec.String

readInput = readFile "input.txt"

readInputList = words <$> readFile "input.txt"

solve :: (String -> Int) -> IO Int
solve solution = solution <$> readInput

solveList :: ([String] -> Int) -> IO Int
solveList solution = solution <$> readInputList

int :: Parser Int
int = read <$> many1 digit
