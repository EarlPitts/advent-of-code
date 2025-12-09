module Day09 where

import AoC.Utils
import Control.Monad
import Data.Bifunctor
import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe

data Pos = Pos {r :: Int, c :: Int} deriving (Show, Eq)

parse :: String -> [Pos]
parse = fmap pPos . lines
  where
    pPos posStr = case splitOn "," posStr of
      [x, y] -> Pos (read x) (read y)

distance :: Pos -> Pos -> Int
distance (Pos x y) (Pos x' y') =
  abs (x - x') + abs (y - y')

distances :: [Pos] -> [(Pos, Pos)]
distances bs =
  [ ((x, y), distance x y)
  | x <- bs,
    y <- delete x bs
  ]
    & sortOn snd
    & fmap fst

recSize :: (Pos, Pos) -> Int
recSize ((Pos x y), (Pos x' y')) = ((abs (x - x')) + 1) * ((abs (y - y')) + 1)

odds :: [a] -> [a]
odds [] = []
odds (x : _ : xs) = x : odds xs

polygon :: [Pos] -> [Pos]
polygon ps = undefined

edges :: [Pos] -> [Pos]
edges ps = join $ fmap (uncurry connect) ends
  where
    ends = concat $ fmap (\p -> zip (repeat p) $ take 2 $ sortOn (distance p) $ connectable (delete p ps) p) ps

-- isInside :: Int -> [Pos] -> Pos -> Bool
-- isInside n ps p@(Pos x y) =
--   if p `elem` ps
--     then True
--     else (\(a, b, c, d) -> if a > 0 && b > 0 && c > 0 && d > 0 then True else False) $ (length $ filter down ps, length $ filter up ps, length $ filter right ps, length $ filter left ps)
--   where
--     left (Pos x' y') = x' == x && y <= y'
--     right (Pos x' y') = x' == x && y > y'
--     down (Pos x' y') = y' == y && x > x'
--     up (Pos x' y') = x' == x && x <= x'

-- isInside' :: Int -> [Pos] -> Pos -> Bool
isInside n ps p@(Pos x y) =
  if p `elem` ps
    then True
    else
      if all
        (> 0)
        ( fmap
            length
            [ filter down ps,
              filter up ps,
              filter right ps,
              filter left ps
            ]
        )
        then True
        else False
  where
    left (Pos x' y') = x' == x && y <= y'
    right (Pos x' y') = x' == x && y > y'
    down (Pos x' y') = y' == y && x > x'
    up (Pos x' y') = y' == y && x <= x'

connect :: Pos -> Pos -> [Pos]
connect (Pos x y) (Pos x' y') =
  if x == x'
    then Pos x <$> [y .. y']
    else (flip Pos) y <$> [x .. x']

connectable :: [Pos] -> Pos -> [Pos]
connectable ps (Pos x y) = filter f ps
  where
    f (Pos x' y') = x' == x || y' == y

poses :: [Pos]
poses = [Pos c r | r <- [0 .. 14], c <- [0 .. 14]]

-- solution :: [Pos] -> Int
-- solution ps = chunksOf 15 $ fmap (\x -> if x then '#' else '.') $ fmap (isInside 15 (edges ps)) poses
--
solution ps = length <$> find (all (isInside 100000 (edges ps))) $ fmap rectangle $ reverse $ distances ps

-- solution ps = find (all (isInside 15 ps)) $ fmap rectangle $ reverse $ distances ps
-- solution ps = chunksOf 15 $ fmap (\p -> if p `elem` rectangle ((Pos 9 5),(Pos 2 3)) then '#' else '.') poses
-- solution ps = fmap (isInside 15 ps) $ rectangle ((Pos 9 5),(Pos 2 3))
-- solution ps = rectangle ((Pos 9 5),(Pos 2 3))
-- solution ps = (isInside 15 ps) (Pos 5 2)

rectangle :: (Pos, Pos) -> [Pos]
rectangle ((Pos x y), (Pos x' y')) = Pos <$> [min x x' .. max x x'] <*> [min y y' .. max y y']

-- solution ps = recSize $ last $ distances ps

main :: IO ()
main = do
  -- ps <- parse <$> readExample
  ps <- parse <$> readInput
  print $ solution ps
  -- void $ traverse print $ solution ps

-- print $ length $ edges ps
