module Day04 where

import AoC.Utils
import Data.List
import Data.Maybe

data Space = Empty | Paper deriving (Show, Eq)

type Grid a = [[a]]

data Pos = Pos {x :: Int, y :: Int} deriving (Show, Eq)

parse :: String -> Grid Space
parse = (fmap . fmap) pSpace . lines
  where
    pSpace '.' = Empty
    pSpace '@' = Paper

countPaper :: Grid Space -> Int
countPaper = length . filter (== Paper) . concat

remove :: Grid Space -> Grid Space
remove grid = (fmap . fmap) f numbered
  where
    numbered = indexGrid grid
    f (r, c, tile) =
      if (tile == Paper)
        && (length $ filter ((==) Paper) $ adjacent grid (Pos r c)) < 4
        then Empty
        else tile

indexGrid :: [[a]] -> [[(Int, Int, a)]]
indexGrid grid =
  [ [(r, c, elem) | (c, elem) <- zip [0 ..] row]
  | (r, row) <- zip [0 ..] grid
  ]

adjacent :: Grid Space -> Pos -> [Space]
adjacent grid (Pos row col) =
  catMaybes
    [ grid !? (row + dr) >>= (!? (col + dc))
    | dr <- [-1, 0, 1],
      dc <- [-1, 0, 1],
      (dr, dc) /= (0, 0)
    ]

solution :: Grid Space -> Int
solution grid = startCount - endCount
  where
    startCount = countPaper grid
    endState = remove grid
    endCount = countPaper endState

solution' :: Grid Space -> Int
solution' grid = startCount - endCount
  where
    startCount = countPaper grid
    endState = fixpoint $ iterate remove grid
    endCount = countPaper endState

main :: IO ()
main = do
  grid <- parse <$> readInput
  print "Part 1"
  print $ solution grid
  print "Part 2"
  print $ solution' grid
