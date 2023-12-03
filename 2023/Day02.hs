import Text.Parsec
import Text.Parsec.String

data Cubes = Green Int | Blue Int | Red Int deriving (Eq,Show)

newtype Draw = Draw [Cubes] deriving (Eq,Show)
newtype Game = Game [Draw] deriving (Eq,Show)

p :: Parser [Game]
p = sepEndBy pLine newline

pLine :: Parser Game
pLine = do
  string "Game " >> many1 digit >> string ": "
  Game <$> sepBy pDraw (char ';' >> space)

pDraw :: Parser Draw
pDraw = Draw <$> sepBy pCubes (char ',' >> space)

pCubes :: Parser Cubes
pCubes = do
  n <- pInt
  space
  c <- choice (string <$> ["blue", "red", "green"])
  return $ mkCubes c n

pInt :: Parser Int
pInt = read <$> many1 digit

mkCubes :: String -> Int -> Cubes
mkCubes "blue"  n = Blue n
mkCubes "green" n = Green n
mkCubes "red"   n = Red n

validGame :: Game -> Bool
validGame (Game gs) = all validDraw gs

validDraw :: Draw -> Bool
validDraw (Draw cs) = all validCubes cs

validCubes :: Cubes -> Bool
validCubes (Blue n)  = n <= 14
validCubes (Red n)   = n <= 12
validCubes (Green n) = n <= 13

main :: IO ()
main = do
  input <- readFile "input"
  let (Right d) = parse p "" input
  let games = zip [1..] d
  let valid = filter (\(_,g) -> validGame g) games
  print $ sum (fst <$> valid)
