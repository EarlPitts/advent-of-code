import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid
import Text.Parsec
import Text.Parsec.String
import Utils

type Seed = Int

data Rule = Rule Int Int Int deriving (Show)

pRule :: Parser Rule
pRule = do
  to <- int <* space
  from <- int <* space
  range <- int
  return (Rule to from range)

pSeeds :: Parser [Seed]
pSeeds = string "seeds: " *> sepEndBy1 int space <* newline

pMap :: Parser (Int -> Int)
pMap = do
  many (alphaNum <|> char '-' <|> space)
  char ':'
  newline
  combine . fmap mkMap <$> sepEndBy1 pRule newline

p :: Parser ([Seed], Endo Int)
p = do
  seeds <- pSeeds
  fs <- fmap Endo <$> sepEndBy pMap newline
  return (seeds, mconcat (reverse fs))

mkMap :: Rule -> (Int -> Int)
mkMap (Rule to from range) n =
  if n >= from && n <= from + range
    then to + (n - from)
    else n

combine :: [Int -> Int] -> (Int -> Int)
combine fs n = fromMaybe n (find (/= n) results)
  where
    results = fs <*> [n]

solution1 :: [Seed] -> (Int -> Int) -> Int
solution1 seeds f = minimum (f <$> seeds)

solution2 :: [Seed] -> (Int -> Int) -> Int
solution2 seeds f = minimum (f <$> allSeeds)
  where
    allSeeds = concatMap (\[f, r] -> [f .. f + r]) $ chunksOf 2 seeds

main = do
  input <- readInput
  let (Right (seeds, Endo f)) = parse p "" input
  print seeds
  print $ solution1 seeds f
  print $ solution2 seeds f
