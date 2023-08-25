import Numeric (showIntAtBase)
import Data.Char (intToDigit)

nextA :: Int -> Int
nextA = flip mod 2147483647 . (* 16807)

nextB :: Int -> Int
nextB = flip mod 2147483647 . (* 48271)

highBits :: Int -> String
highBits n = take 16 (reverse bin)
  where bin = showIntAtBase 2 intToDigit n ""

-- This works, but its horribly slow and uses a ton of memory
-- Also have an off-by-one error, which is weird
main :: IO ()
main = do
  initA <- read <$> getLine
  initB <- read <$> getLine
  let a = take 40000000 $ tail $ iterate nextA initA
  let b = take 40000000 $ tail $ iterate nextB initB
  let pairs = zip a b
  print $ length $ filter (\(x, y) -> highBits x == highBits y) pairs
