module Day12 where

import Data.List
import Data.List.Split (splitOn)
import AoC.Utils

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

perms :: [Spring] -> [[Spring]]
perms = foldr f [[]]
  where
    f Ugly ps = ((Good :) <$> ps) <> ((Bad :) <$> ps)
    f s ps = (s :) <$> ps

check :: [Int] -> [Spring] -> Bool
check damaged ss = grps == damaged
  where
    grps = fmap length $ filter ((== Bad) . head) $ group ss

count :: Record -> Int
count (Record ss damaged) = length $ filter (check damaged) (perms ss)

solution :: [Record] -> Int
solution = sum . fmap count

main = do
  -- let rs = parse input
  rs <- parse <$> readInput
  print $ solution rs
