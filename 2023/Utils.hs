module Utils where

import Text.Parsec
import Text.Parsec.String

readInput = readFile "input.txt"

readInputList = words <$> readFile "input.txt"

solve :: (String -> Int) -> IO Int
solve solution = solution <$> readInput

solveList :: ([String] -> Int) -> IO Int
solveList solution = solution <$> readInputList

iterateUntilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM p f a = do
  a' <- f a
  if p a' then return a' else iterateUntilM p f a'

int :: Parser Int
int = read <$> many1 digit
