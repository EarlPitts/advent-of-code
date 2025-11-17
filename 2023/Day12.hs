module Day12 where

import AoC.Utils
import Control.Monad
import Criterion.Main
import qualified Data.Array as A
import Data.List
import Data.List.Split (splitOn)

input =
  "???.### 1,1,3\n\
  \.??..??...?##. 1,1,3\n\
  \?#?#?#?#?#?#?#? 1,3,1,6\n\
  \????.#...#... 4,1,1\n\
  \????.######..#####. 1,6,5\n\
  \?###???????? 3,2,1"

data Record = Record [Spring] [Int] deriving (Eq, Show)

data Spring = Good | Bad | Ugly deriving (Show, Eq, Ord)

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
count (Record ss damaged) = arrangements' ss damaged

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

-- https://github.com/jnoliv/Advent-of-Haskell/blob/87c1fad6bfd093f4ced5e709f8a7379b665ccf08/src/2023/day12.hs#L55
arrangements' :: [Spring] -> [Int] -> Int
arrangements' ss gs = go 0 0
  where
    ssLen = length ss
    gsLen = length gs

    arrs = A.listArray ((0, 0), (ssLen, gsLen)) [go i j | i <- [0 .. ssLen], j <- [0 .. gsLen]]


    bads indS indG
      | Good `elem` (drop indS (take (gs !! indG))) = 0
      | indS + (gs !! indG) == ssLen = 1
      | indS !! (gs !! indG) == Bad = 0
      | otherwise = (A.!) arrs (indS + (gs !! indG) + 1,indG + 1)

    go indS indG
      | indS == ssLen && indG == gsLen = 1
      | Bad `elem` drop indS ss = 0
      | not (indS == ssLen && indG == gsLen) = 0
      | ss !! indS == Good = (A.!) arrs (indS + 1, indG)
      -- | curr == Ugly = (A.!) arrs (indS + 1, indG) + (
      -- | ss !! indS == Bad = if then (A.!) arrs (indS + 
      | otherwise = 1

    -- go (Bad : ss) (g : gs) =
    --   if g <= length (Bad : ss)
    --     && notElem Good (take g (Bad : ss))
    --     && (g == length (Bad : ss) || ((length (Bad : ss) > g) && ((Bad : ss) !! g /= Bad)))
    --     then (M.!) arrs (drop g ss, gs)
    --     else 0
    -- go (Ugly : ss) gs = (M.!) arrs (Bad : ss, gs) + (M.!) arrs (Good : ss, gs)

unfold :: Record -> Record
unfold (Record ss gs) =
  Record (intercalate [Ugly] $ replicate 5 ss) (concat $ replicate 5 gs)

main = do
  -- let rs = parse input
  rs <- parse <$> readInput
  let unfolded = unfold <$> rs
  print unfolded
  print $ solution unfolded

benchmark :: IO ()
benchmark = do
  rs <- parse <$> readInput
  defaultMain
    [ bgroup
        "arrangements"
        [ bench "backtracking" $ whnf (arrangements (replicate 25 Ugly)) [1, 5, 6, 3]
        -- bench "dp" $ whnf fib'' 30
        ]
    ]
