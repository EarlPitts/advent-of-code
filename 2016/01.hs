data Turn = L | R deriving Show
data Ins = Ins Turn Int deriving Show

data Dir = N | W | S | E deriving Show
data Coord = Coord Int Int deriving Show
data State = State Dir Coord deriving Show

parse :: String -> [Ins]
parse input = parseIns <$> (words . filter (/= ',')) input

parseIns :: String -> Ins
parseIns (d:n) = case d of
  'R' -> Ins R (read n)
  'L' -> Ins L (read n)

step :: State -> Ins -> State
step (State N (Coord x y)) (Ins L n) = State W (Coord (x - n) y)
step (State N (Coord x y)) (Ins R n) = State E (Coord (x + n) y)
step (State W (Coord x y)) (Ins L n) = State S (Coord x (y - n))
step (State W (Coord x y)) (Ins R n) = State N (Coord x (y + n))
step (State S (Coord x y)) (Ins L n) = State E (Coord (x + n) y)
step (State S (Coord x y)) (Ins R n) = State W (Coord (x - n) y)
step (State E (Coord x y)) (Ins L n) = State N (Coord x (y + n))
step (State E (Coord x y)) (Ins R n) = State S (Coord x (y - n))

manhattan :: Coord -> Int
manhattan (Coord x y) = x + y

main :: IO ()
main = do
  input <- parse <$> readFile "input"
  let (State N finalCoord) = foldl step (State N (Coord 0 0)) input
  print $ manhattan finalCoord
