import Data.Char
import Data.Either
import Data.List
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

data ProgramRaw = ProgramRaw {
  name :: String,
  weight :: Integer,
  children :: [String]
} deriving (Show, Eq)

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

findRoot :: [ProgramRaw] -> [String]
findRoot ps = (\\) allNodes internalNodes
  where internalNodes = concatMap children ps
        allNodes = fmap name ps

solve str = show $ findRoot nodes
  where
    progs = rights (fmap (parse p "") (lines str))
    (nodes, leafs) = partition (not . null . children) progs

type Name = String
type Weight = Integer

data Tree = Node Name Weight [Tree] | Leaf Weight

main = interact solve
