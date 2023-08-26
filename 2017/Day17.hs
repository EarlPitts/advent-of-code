import Control.Monad.State
import Debug.Trace

type Circular = ([Int], Index)

type Value = Int

type Index = Int

insert :: Index -> Value -> Circular -> Circular
insert n v (l, pos) = (take i l ++ [v] ++ drop i l, i)
  where
    i = mod (n + pos) (length l) + 1

next :: Circular -> Int
next (l, pos) = l !! (pos + 1)

step :: Int -> Circular -> State Int Circular
step n c = do
  val <- get
  if val == 2018
    then return c
    else do
      let next = insert n val c
      put (val + 1)
      step n next

main :: IO ()
main = do
  input <- read <$> getLine
  print $ next $ evalState (step input ([0],0)) 1
