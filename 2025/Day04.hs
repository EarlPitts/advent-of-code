module Day04 where

import AoC.Utils
import Control.Comonad
import Control.Comonad.Store
import Control.Monad
import Control.Monad.State
import Data.List
import Data.Maybe
-- import Grid

data Space = Empty | Paper deriving (Show, Eq)

type Grid a = [[a]]

data Pos = Pos {x :: Int, y :: Int} deriving (Show, Eq)

parse :: String -> Grid Space
parse = (fmap . fmap) pSpace . lines
  where
    pSpace '.' = Empty
    pSpace '@' = Paper


solution :: Grid Space -> Int
solution grid = startCount - endCount
  where
    startCount = countPaper grid
    endState = fixpoint $ iterate remove grid
    endCount = countPaper endState

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
adjacent grid (Pos row col) = catMaybes [above, below, left, right, upperLeft, upperRight, bottomLeft, bottomRight]
  where
    above = (grid !? (row - 1)) >>= (\r -> r !? col)
    below = (grid !? (row + 1)) >>= (\r -> r !? col)
    left = (grid !! row) !? (col - 1)
    right = (grid !! row) !? (col + 1)
    upperRight = (grid !? (row - 1)) >>= (\r -> r !? (col + 1))
    upperLeft = (grid !? (row - 1)) >>= (\r -> r !? (col - 1))
    bottomLeft = (grid !? (row + 1)) >>= (\r -> r !? (col - 1))
    bottomRight = (grid !? (row + 1)) >>= (\r -> r !? (col + 1))

-- solution :: Grid Space -> Int
-- solution grid = beginCnt - endCnt
--   where
--     beginCnt = countPapers grid
--     endState = grid =>> iteration
--     endCnt = countPapers endState

-- iteration :: Grid Space -> Space
-- iteration = count =>= remove
--
fixpoint :: (Eq a) => [a] -> a
fixpoint (x : y : as) = if x == y then x else fixpoint (y : as)
--
-- -- remove :: Grid Space -> Space
-- -- remove grid = if cnt < 4 then Empty else focus grid
-- --   where
-- --     cnt = length $ filter ((==) Paper) $ fmap focus $ adjacent grid
--
countPapers :: Grid Space -> Int
countPapers = length . filter (== Paper) . concat
--
-- remove :: Grid (Space, Int) -> Space
-- remove grid = if n < 4 then Empty else s
--   where
--     (s, n) = focus grid
--
-- count :: Grid Space -> (Space, Int)
-- count grid = (focus grid, length $ filter ((==) Paper) $ fmap focus $ adjacent grid)

main :: IO ()
main = do
  grid <- parse <$> readInput
  print "Part 1"
  print $ solution grid
  print "Part 2"
  print $ solution grid
