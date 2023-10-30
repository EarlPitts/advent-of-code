import Data.Char
import Data.List
import Data.Ord
import Text.Parsec
import Text.Parsec.String

type Name = String
type ID = Int
type Checksum = String
data Room = Room
  { name :: Name,
    roomId :: ID,
    checksum :: Checksum
  }
  deriving Show

pName :: Parser Name
pName = concat <$> endBy1 (many1 lower) (char '-')

pInt :: Parser Int
pInt = many1 digit >>= return . read

pChecksum :: Parser Checksum
pChecksum = char '[' *> many1 lower <* char ']'

pRoom :: Parser Room
pRoom = do
  name <- pName
  id <- pInt
  checksum <- pChecksum
  return $ Room name id checksum

p :: Parser [Room]
p = sepEndBy pRoom newline <* eof

validate :: Room -> Bool
validate (Room name _ checksum) =
  (head <$> take 5 (sortOn (Down . length) (group (sort name)))) == checksum

betterOrd :: Char -> Int
betterOrd c = i
  where
    Just i = elemIndex c ['a'..'z']

betterChr :: Int -> Char
betterChr i = ['a'..'z'] !! i

shift :: Room -> Room
shift (Room name id _) = Room newName id ""
  where
    newName = betterChr . (flip mod 26 . (+) id) . betterOrd <$> name

checkNorthPoleObjects :: Room -> Bool
checkNorthPoleObjects (Room name id _) =
  isInfixOf "northpole" name

main :: IO ()
main = do
  Right input <- parse p "" <$> readFile "input"
  let reals = filter validate input
  print $ sum (roomId <$> reals)
  print $ filter checkNorthPoleObjects $ shift <$> reals
