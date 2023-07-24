import Data.List
import Data.Maybe
import Debug.Trace
import Control.Monad.State

type Memory = [Int]

type Index = Int

maxIndex :: Memory -> Index
maxIndex m = fromJust $ elemIndex max m
  where
    max = maximum m

incBlock :: Memory -> Index -> Memory
incBlock m i = take i m ++ [n + 1] ++ drop (i + 1) m
  where
    n = m !! i

redist :: Memory -> Index -> Int -> Memory
redist mem i 0 = mem
redist mem i n =
  if lastBlock
    then redist (incBlock mem 0) 1 (n - 1)
    else redist (incBlock mem i) (i + 1) (n - 1)
  where
    lastBlock = length mem == i

step :: Memory -> Memory
step m = redist m' (i+1) n
  where
    i = maxIndex m
    n = m !! maxIndex m
    m' = take i m ++ [0] ++ drop (i+1) m
    
solve :: Memory -> State [Memory] Int
solve m = do
  seen <- get
  let newMem = step m
  if elem newMem seen
  then return (length seen + 1)
  else do
    put (newMem : seen)
    solve newMem

solution :: String -> String
solution s = show (evalState (solve' mem) [])
  where
    mem = fmap read (lines s)

solve' :: Memory -> State [Memory] Int
solve' m = do
  seen <- get
  let newMem = step m
  if elem newMem seen
  then return $ length seen - fromJust (elemIndex newMem (reverse seen))
  else do
    put (newMem : seen)
    solve' newMem

main = interact solution
