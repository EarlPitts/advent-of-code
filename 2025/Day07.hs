module Day07 where

import AoC.Utils
import Control.Monad.State
import Data.List
import qualified Data.Map as M

data Pos = Pos {r :: Int, c :: Int} deriving (Show, Eq, Ord)

data Input = Input Pos [Pos] Int deriving (Show, Eq)

parse :: String -> Input
parse str = Input (Pos 0 startCol) poses (length ls)
  where
    ls = lines str
    Just startCol = elemIndex 'S' (head ls)
    indexed = concat $ indexGrid ls
    poses = fmap f $ filter (\(_, _, c) -> c == '^') indexed
    f (r, c, _) = Pos r c

down :: Pos -> Pos
down (Pos r c) = Pos (r + 1) c

splitBeam :: Pos -> [Pos]
splitBeam (Pos r c) = [Pos r (c - 1), Pos r (c + 1)]

count :: Pos -> [Pos] -> Int -> State (M.Map Pos Int) Int
count beam@(Pos r c) ss len =
  if r == len
    then pure 0
    else
      if next `elem` ss
        then do
          map <- get
          case M.lookup next map of
            Just c -> pure c
            Nothing ->
              let [a, b] = splitBeam next
               in do
                    result <- (+) <$> (count a ss len) <*> (count b ss len)
                    modify $ M.insert next (result + 1)
                    pure (result + 1)
        else count next ss len
  where
    next = down beam

solution (Input start ss len) = runState (count start ss len) (M.empty)

main :: IO ()
main = do
  -- ps <- parse <$> readExample
  ps <- parse <$> readInput
  let (p2, p1) = solution ps
  print $ "Part 1"
  print $ length p1
  print $ "Part 2"
  print $ succ p2
