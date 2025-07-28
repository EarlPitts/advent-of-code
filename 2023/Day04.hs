import Text.Parsec
import Text.Parsec.String
import Utils

input =
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
  \Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
  \Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
  \Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
  \Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
  \Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11\n"

data Card = Card Int [Int] [Int] deriving (Show)

p :: Parser [Card]
p = sepEndBy card newline

card :: Parser Card
card = do
  string "Card" >> spaces
  id <- int
  string ":" >> spaces
  winning <- sepEndBy int spaces
  string "|" >> spaces
  have <- sepBy int (many1 (char ' '))
  return $ Card id winning have

solution1 :: [Card] -> Int
solution1 = sum . fmap (\n -> 2 ^ (n - 1)) . filter (/= 0) . fmap numOfWinning

numOfWinning :: Card -> Int
numOfWinning (Card _ winning have) = length $ [w | w <- winning, h <- have, h == w]

main = do
  input <- readInput
  let (Right cards) = parse p "" input
  print cards
  print $ solution1 cards
