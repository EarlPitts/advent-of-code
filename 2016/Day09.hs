module Day09 where

import Control.Monad
import Text.Parsec
import Text.Parsec.String

newtype Data = Data [Either Repeated String] deriving (Eq, Show)
data Repeated = Repeated Int String deriving (Eq, Show)

instance Semigroup Data where
  (<>) (Data d) (Data d') = Data (d <> d')

data Marker = Marker Int Int deriving (Eq, Show)

p :: Parser Data
p = foldr (<>) (Data []) <$> many1 (parseMarked <|> parseUnmarked)
  
parseUnmarked :: Parser Data
parseUnmarked = do
  chars <- many1 upper
  return $ Data [Right chars]

parseMarked :: Parser Data
parseMarked = do
  m@(Marker l t) <- parseMarker
  chars <- replicateM l anyChar
  return $ Data [Left (Repeated t chars)]

parseMarker :: Parser Marker
parseMarker = do
  char '('
  l <- parseInt
  char 'x'
  t <- parseInt
  char ')'
  return $ Marker l t

parseInt :: Parser Int
parseInt = read <$> many1 digit

decompress :: Data -> String
decompress (Data d) = foldr f [] d
  where
    f (Right s) acc = s ++ acc
    f (Left (Repeated n s)) acc = concat (replicate n s) ++ acc

-- You have to decompress until there is no marker left :)
main :: IO ()
main = do
  Right input <- parse p "" <$> readFile "input"
  let once = decompress input
  let (Right twice) = decompress <$> parse p "" once
  print $ length once
  print $ length twice
