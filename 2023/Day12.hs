module Day12 where

import AoC.Utils
import Data.List.Split (splitOn)

input =
  "???.### 1,1,3\n\
  \.??..??...?##. 1,1,3\n\
  \?#?#?#?#?#?#?#? 1,3,1,6\n\
  \????.#...#... 4,1,1\n\
  \????.######..#####. 1,6,5\n\
  \?###???????? 3,2,1"

data Record = Record [Spring] [Int] deriving (Eq, Show)

data Spring = Good | Bad | Ugly deriving (Show, Eq)

parseSpring '.' = Good
parseSpring '#' = Bad
parseSpring '?' = Ugly

parse :: String -> [Record]
parse = fmap parseLine . lines

parseLine :: String -> Record
parseLine str = Record springs damaged
  where
    [springsStr, damagedStr] = words str
    springs = parseSpring <$> springsStr
    damaged = read <$> splitOn "," damagedStr

count :: Record -> Int
count (Record ss damaged) = arrangements ss damaged

solution :: [Record] -> Int
solution = sum . fmap count

arrangements :: [Spring] -> [Int] -> Int
arrangements ss [] = if Bad `elem` ss then 0 else 1
arrangements [] gs = if null gs then 1 else 0
arrangements (Good : ss) gs = arrangements ss gs
arrangements (Bad : ss) (g : gs) =
  if g <= length (Bad : ss)
    && notElem Good (take g (Bad : ss))
    && (g == length (Bad : ss) || ((length (Bad : ss) > g) && ((Bad : ss) !! g /= Bad)))
    then arrangements (drop g ss) gs
    else 0
arrangements (Ugly : ss) gs = arrangements (Bad : ss) gs + arrangements (Good : ss) gs

main = do
  -- let rs = parse input
  rs <- parse <$> readInput
  print $ solution rs
