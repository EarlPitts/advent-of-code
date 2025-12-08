module Day08 where

import AoC.Utils
import Data.List
import Data.List.Split

data Box = Box {x :: Int, y :: Int, z :: Int} deriving (Show, Eq)

type Circuit = [Box]

parse :: String -> [Box]
parse str = fmap pBox $ lines str
  where
    pBox boxStr = case splitOn "," boxStr of
      [x, y, z] -> Box (read x) (read y) (read z)

distance :: Box -> Box -> Float
distance (Box x y z) (Box x' y' z') =
  sqrt (fromIntegral (((x - x') ^ 2) + ((y - y') ^ 2) + ((z - z') ^ 2)))

distances :: [Box] -> [(Box, Box)]
distances bs =
  fmap (\(b, b', _) -> (b, b')) $ sortOn (\(_, _, d) -> d) $ [(x, y, distance x y) | x <- bs, y <- delete x bs]

findCircuit :: Box -> [Circuit] -> Circuit
findCircuit b cs = case find (elem b) cs of
  Just c -> c

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
  product $ take 3 $ sortBy (flip compare) $ fmap length $ snd $ iterate connect (ds, cs) !! 1000
  where
    cs = fmap singleton bs
    ds = odds $ distances bs

solution' :: [Box] -> Int
solution' bs = b.x * b'.x
  where
    cs = fmap singleton bs
    ds = odds $ distances bs
    Just idx = findIndex (== 1) $ fmap (length . snd) $ iterate connect (ds, cs)
    (b, b') = ds !! idx

main :: IO ()
main = do
  bs <- parse <$> readInput
  print "Part 1"
  print $ solution bs
  print "Part 2"
  print $ solution' bs
