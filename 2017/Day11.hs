import Data.Bifunctor
import Data.Either
import Text.Parsec
import Text.Parsec.String

type Coord = (Int, Int)

-- We could use Endo from Data.Monoid too, so we wouldn't have to
-- use foldr explicitly in path
type Move = Coord -> Coord

n :: Move
n = second succ

s :: Move
s = second pred

e :: Move
e = first succ

w :: Move
w = first pred

nw :: Move
nw = n . w

ne :: Move
ne = n . e

se :: Move
se = s . e

sw :: Move
sw = s . w

p :: Parser [Move]
p = sepBy dirParser (char ',')

dirParser :: Parser Move
dirParser =
  pairUp
    <$> ( try (string "nw")
            <|> try (string "ne")
            <|> try (string "se")
            <|> try (string "sw")
            <|> string "n"
            <|> string "s"
            <|> string "w"
            <|> string "e"
        )
  where
    pairUp "n" = n
    pairUp "s" = s
    pairUp "w" = w
    pairUp "e" = e
    pairUp "nw" = nw
    pairUp "ne" = ne
    pairUp "se" = se
    pairUp "sw" = sw

path :: [Move] -> Move
path = foldl (.) id

allPaths :: [Move] -> [Move]
allPaths = scanl (.) id

-- distance :: Coord -> Int
-- distance (x, y) = go (abs x, abs y)
--   where
--     go (0, y) = y
--     go (x, 0) = x
--     go (x, y) = succ $ go (pred x, pred y)

distance :: Coord -> Int
distance (x, y) = max x y

main :: IO ()
main = do
  input <- getContents
  let moves = fromRight [] (parse p "" input)
  let coord = path moves (0, 0)
  let allCoords = allPaths moves <*> [(0,0)]
  print $ distance coord
  print $ maximum $ distance <$> allCoords
