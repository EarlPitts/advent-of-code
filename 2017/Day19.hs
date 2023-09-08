import Control.Monad.State as S
import Data.List
import Debug.Trace
import Text.Parsec
import Text.Parsec.String

data Tile
  = Cross
  | Vert
  | Hor
  | Empt
  | Letter Char
  deriving (Show, Eq)

type Maze = [[Tile]]

cross :: Parser Tile
cross = char '+' >> return Cross

empty :: Parser Tile
empty = char ' ' >> return Empt

horizontal :: Parser Tile
horizontal = char '-' >> return Hor

vertical :: Parser Tile
vertical = char '|' >> return Vert

lett :: Parser Tile
lett = Letter <$> upper

p :: Parser Maze
p = sepBy (many $ choice [empty, horizontal, vertical, lett, cross]) (char '\n')

data Dir = U | D | L | R deriving (Show, Eq)

type Solution = String

type Position = (Int, Int)

solve :: Maze -> Position -> S.State (Dir, Solution) Solution
solve m p = do
  newPos <- step m p
  (dir,sol) <- get
  if lastTile m (trace (show (newPos,dir)) newPos)
  then get >>= (\(_,s) -> return s)
  else solve m newPos

lastTile :: Maze -> Position -> Bool
lastTile m (r,c) = length (filter isEmpty adj) == 3
  where adj = [getTile m (r-1) c, getTile m (r+1) c, getTile m r (c-1), getTile m r (c+1)]
        isEmpty Empt = True
        isEmpty _    = False

step :: Maze -> Position -> S.State (Dir, Solution) Position
step m (r, c) = do
  (dir,sol) <- get
  (r',c') <- newPos m (r,c) dir tile
  (dir,sol) <- get
  if isLetter (getTile m r' c')
  then let (Letter l) = getTile m r' c' in put (dir,l:sol) >> return (r',c')
  else return (r',c')
  where
    tile = getTile m r c
    isLetter (Letter _) = True
    isLetter _          = False

newPos :: Maze -> Position -> Dir -> Tile -> S.State (Dir, Solution) Position
newPos m (r,c) d Cross = case changeDir m (r,c) d of
  U -> get >>= (\(_,s) -> put (U,s) >> return (r-1,c))
  D -> get >>= (\(_,s) -> put (D,s) >> return (r+1,c))
  L -> get >>= (\(_,s) -> put (L,s) >> return (r,c-1))
  R -> get >>= (\(_,s) -> put (R,s) >> return (r,c+1))
newPos _ (r,c) d Empt = error $ show (r,c) <> " " <> show d
newPos _ (r,c) U _ = return (r-1,c)
newPos _ (r,c) D _ = return (r+1,c)
newPos _ (r,c) L _ = return (r,c-1)
newPos _ (r,c) R _ = return (r,c+1)

getTile :: Maze -> Int -> Int -> Tile
getTile m r c = (m !! r) !! c

changeDir :: Maze -> Position -> Dir -> Dir
changeDir m (r,c) U = if getTile m r (c-1) == Empt then R else L
changeDir m (r,c) D = if getTile m r (c-1) == Empt then R else L
changeDir m (r,c) L = if getTile m (r-1) c == Empt then D else U
changeDir m (r,c) R = if getTile m (r-1) c == Empt then D else U

start :: Maze -> Position
start (l : _) = (0, c)
  where
    (Just c) = elemIndex Vert l

main = do
  input <- getContents
  let (Right maze) = parse p "" input
  let startPos = start maze
  print $ reverse (evalState (solve maze startPos) (D,""))
