module Day08 where

import AoC.Utils
import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe

data Box = Box {x :: Int, y :: Int, z :: Int} deriving (Show, Eq)

type Circuit = [Box]

parse :: String -> [Box]
parse = fmap pBox . lines
  where
    pBox boxStr = case splitOn "," boxStr of
      [x, y, z] -> Box (read x) (read y) (read z)

distance :: Box -> Box -> Float
distance (Box x y z) (Box x' y' z') =
  sqrt (fromIntegral (((x - x') ^ 2) + ((y - y') ^ 2) + ((z - z') ^ 2)))

distances :: [Box] -> [(Box, Box)]
distances bs =
  [ ((x, y), distance x y)
  | x <- bs,
    y <- delete x bs
  ]
    & sortOn snd
    & fmap fst

findCircuit :: Box -> [Circuit] -> Circuit
findCircuit b cs = fromJust $ find (elem b) cs

odds :: [a] -> [a]
odds [] = []
odds (x : _ : xs) = x : odds xs

connect :: ([(Box, Box)], [Circuit]) -> ([(Box, Box)], [Circuit])
connect (((b1, b2) : ds), cs) =
  if c1 == c2
    then (ds, cs)
    else (ds, ((c1 <> c2) : (delete c2 (delete c1 cs))))
  where
    c1 = findCircuit b1 cs
    c2 = findCircuit b2 cs

solution :: [Box] -> Int
solution bs =
  iterate connect (ds, cs) !! 1000
    & snd
    & fmap length
    & sortBy (flip compare)
    & take 3
    & product
  where
    cs = fmap singleton bs
    ds = odds $ distances bs

solution' :: [Box] -> Int
solution' bs = b.x * b'.x
  where
    cs = fmap singleton bs
    ds = odds $ distances bs
    Just idx = findIndex (== 1) $ fmap (length . snd) $ iterate connect (ds, cs)
    (b, b') = ds !! (idx - 1)

main :: IO ()
main = do
  bs <- parse <$> readInput
  print "Part 1"
  print $ solution bs
  print "Part 2"
  print $ solution' bs
