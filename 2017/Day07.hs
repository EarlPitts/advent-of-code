import Control.Monad.State as ST
import Data.Either
import Data.List
import Data.Maybe
import Text.Parsec
import Text.Parsec.String

data ProgramRaw = ProgramRaw
  { name :: String,
    weight :: Integer,
    children :: [String]
  }
  deriving (Show, Eq)

num :: Parser Integer
num = do
  n <- many1 digit
  return (read n)

p :: Parser ProgramRaw
p = do
  name <- many lower
  space
  weight <- char '(' *> num <* char ')'
  optional (space >> string "->" >> space)
  children <- sepBy (many1 lower) (char ',' >> space)
  return $ ProgramRaw name weight children

findRoot :: [ProgramRaw] -> ProgramRaw
findRoot ps = head $ filter (\p -> name p == rootName) ps
  where
    rootName = head $ (\\) allNodes internalNodes
    internalNodes = concatMap children ps
    allNodes = fmap name ps

solve str = show $ findRoot nodes
  where
    progs = rights (fmap (parse p "") (lines str))
    (nodes, leafs) = partition (not . null . children) progs

type Name = String

type Weight = Integer

data Tree a = Node a [Tree a] | Leaf a deriving (Show, Eq)

instance Functor Tree where
  fmap f (Node x t) = Node (f x) ((fmap . fmap) f t)
  fmap f (Leaf x) = Leaf (f x)

instance Foldable Tree where
  foldr f z (Node x ts) = f x (foldr (\t z -> foldr f z t) z ts)
  foldr f z (Leaf x) = f x z

solve' str = show $ evalState (f t) []
  where
    t = buildTree (findRoot nodes) progs
    progs = rights (fmap (parse p "") (lines str))
    (nodes, leafs) = partition (not . null . children) progs

findProgs :: [Name] -> [ProgramRaw] -> [ProgramRaw]
findProgs ns = filter (\p -> name p `elem` ns)

buildTree :: ProgramRaw -> [ProgramRaw] -> Tree Weight
buildTree (ProgramRaw _ w []) ps = Leaf w
buildTree (ProgramRaw _ w cs) ps = Node w (fmap (`buildTree` ps) (findProgs cs ps))

oddOne l = if x1 == x2 then last sorted else head sorted
  where
    sorted = sort l
    [x1, x2] = take 2 sorted

allSame l = minimum l == maximum l

-- This is undeniably horrible, but I got bored
f :: Tree Weight -> ST.State [Tree Weight] Weight
f (Node w ts) = do
  if allSame ws
    then do
      ts' <- get
      let ws' = fmap sum ts'
      let wrongIndex' = fromJust $ elemIndex (oddOne ws') ws'
      let diff = abs (oddOne ws' - head ((\\) ws' [oddOne ws']))
      let (Node weight _) = ts' !! wrongIndex'
      return (weight - diff) -- This (-) could be (+)
    else do
      put ts
      f (ts !! wrongIndex)
  where
    ws = fmap sum ts
    wrongIndex = fromJust $ elemIndex (oddOne ws) ws
    isLeaf (Leaf _) = True
    isLeaf _ = False

main = interact solve'
