import Control.Applicative
import Data.Either
import Text.Parsec
import Text.Parsec.String

type Layer = Int
type Depth = Int
type FireWall = [(Layer, Depth)]

p :: Parser FireWall
p = many1 pLine

pLine :: Parser (Layer, Depth)
pLine = do
  layer <- read <$> many1 digit
  char ':'
  space
  depth <- read <$> many1 digit
  newline
  return (layer, depth)

cycleLength :: Depth -> Int
cycleLength 1 = 1
cycleLength 2 = 2
cycleLength n = 2 * n - 2

-- Gives back the pentaly for each round
step :: Int -> FireWall -> Int
step round fw = case lookup round fw of
  Nothing -> 0
  Just depth ->
    if mod round (cycleLength depth) == 0
      then depth * round
      else 0

main :: IO ()
main = do
  input <- getContents
  let fw = fromRight (error "Oh no!") (parse p "" input)
  let numOfRounds = (fst $ last fw :: Int)
  let penalties = liftA2 step (ZipList [0 .. numOfRounds + 1]) (ZipList (replicate (numOfRounds + 1) fw))
  print $ sum (getZipList penalties)
