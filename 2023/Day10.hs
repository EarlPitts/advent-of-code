module Day10 where

import Control.Monad.State
import Grid
import Utils

input =
  ".....\n\
  \.S-7.\n\
  \.|.|.\n\
  \.L-J.\n\
  \....."

data Pipe = NS | EW | NE | NW | SW | SE | Start | Blank deriving (Eq, Show)

parse :: Char -> Pipe
parse '|' = NS
parse '-' = EW
parse 'L' = NE
parse 'J' = NW
parse '7' = SW
parse 'F' = SE
parse 'S' = Start
parse '.' = Blank

parseMaze :: String -> Grid Pipe
parseMaze =
  fromLists
    . fmap (fmap parse)
    . lines

moveToStart :: Grid Pipe -> Grid Pipe
moveToStart maze = setPos start maze
  where
    start = findPos (== Start) maze

data Dir = U | D | L | R deriving (Eq, Show)

step :: Grid Pipe -> State Dir (Grid Pipe)
step maze = do
  dir <- get
  let newMaze = case dir of
        U -> moveUp maze
        D -> moveDown maze
        L -> moveLeft maze
        R -> moveRight maze
  modify $ changeDir (focus newMaze)
  return newMaze

changeDir :: Pipe -> Dir -> Dir
changeDir EW L = L
changeDir NE L = U
changeDir SE L = D
changeDir EW R = R
changeDir NW R = U
changeDir SW R = D
changeDir NS U = U
changeDir SW U = L
changeDir SE U = R
changeDir NS D = D
changeDir NE D = R
changeDir NW D = L

run :: Grid Pipe -> State Dir [Grid Pipe]
run = iterateUntilM ((== Start) . focus) step

solution :: Grid Pipe -> Int
solution maze = div (length (evalState (run maze) L)) 2

main :: IO ()
main = do
  maze <- moveToStart . parseMaze <$> readInput
  -- let maze = moveToStart $ parseMaze input
  print $ solution maze
