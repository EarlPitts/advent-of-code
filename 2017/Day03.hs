import Prelude hiding (Left, Right)

data Direction = Left | Right | Up | Down deriving (Show, Eq)

step ((x,y), Left)  = if abs x == abs y then ((x, y-1), Down) else ((x-1, y), Left)
step ((x,y), Down)  = if abs x == abs y then ((x+1, y), Right) else ((x, y-1), Down)
step ((x,y), Right) = if abs x >  abs y then ((x, y+1), Up) else ((x+1, y), Right)
step ((x,y), Up)    = if abs x == abs y then ((x-1, y), Left) else ((x, y+1), Up)

solution :: Int -> Int
solution n = add $ iterate step orig !! (n-1)
  where orig = ((0,0), Right)
        add ((x,y),_) = abs x + abs y

type Pos = (Int, Int)

data Field = Field
  { pos :: Pos
  , dir :: Direction
  , val  :: Int
  } deriving (Show, Eq)

adjacent :: Pos -> Pos -> Bool
adjacent (x,y) (x',y') = elem (x',y') $ (,) <$> (fs <*> [x]) <*> (fs <*> [y])
  where fs = [id,succ,pred]

next :: [Field] -> Field
next (f@(Field p d v):fs) = Field newpos newdir newval
  where (newpos, newdir) = step (p, d)
        adj = filter (adjacent newpos . pos) (f:fs)
        newval = sum (val <$> adj)
        
solution' fs'@(f:fs) = if val f > 277678 then f else solution' (next fs':fs')
