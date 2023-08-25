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
main :: IO ()
main = do
  initA <- read <$> getLine
  initB <- read <$> getLine
  let a = tail $ iterate nextA initA
  let b = tail $ iterate nextB initB
  let filteredA = filter (\x -> mod x 4 == 0) a
  let filteredB = filter (\x -> mod x 8 == 0) b
  let pairs = zip a b
  let filteredPairs = zip filteredA filteredB
  print $ length $ filter (\(x, y) -> highBits x == highBits y) (take 40000000 pairs)
  print $ length $ filter (\(x, y) -> highBits x == highBits y) (take 5000000 filteredPairs)
