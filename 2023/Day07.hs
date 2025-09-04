module Day07 where

import Control.Category ((>>>))
import Data.List
import Data.Ord
import Utils

input =
  "32T3K 765\n\
  \T55J5 684\n\
  \KK677 28\n\
  \KTJJT 220\n\
  \QQQJA 483\n"

data Card = Face F | Number Int deriving (Show, Eq)

data F = T | J | Q | K | A deriving (Show, Eq, Ord, Read)

data Hand
  = High [Card]
  | OnePair [Card]
  | TwoPair [Card]
  | ThreeKind [Card]
  | FullHouse [Card]
  | FourKind [Card]
  | FiveKind Card
  deriving (Show, Eq)

instance Ord Card where
  (<=) (Face f) (Face f') = f <= f'
  (<=) (Number n) (Number n') = n <= n'
  (<=) (Face _) (Number _) = False
  (<=) (Number _) (Face _) = True

instance Ord Hand where
  (<=) (High cs) (High cs') = cs <= cs'
  (<=) (OnePair cs) (OnePair cs') = cs <= cs'
  (<=) (TwoPair cs) (TwoPair cs') = cs <= cs'
  (<=) (ThreeKind cs) (ThreeKind cs') = cs <= cs'
  (<=) (FullHouse cs) (FullHouse cs') = cs <= cs'
  (<=) (FourKind cs) (FourKind cs') = cs <= cs'
  (<=) (FiveKind c) (FiveKind c') = c <= c'
  (<=) (High _) _ = True
  (<=) (OnePair _) (High _) = False
  (<=) (OnePair _) _ = True
  (<=) (TwoPair _) (High _) = False
  (<=) (TwoPair _) (OnePair _) = False
  (<=) (TwoPair _) _ = True
  (<=) (ThreeKind _) (High _) = False
  (<=) (ThreeKind _) (OnePair _) = False
  (<=) (ThreeKind _) (TwoPair _) = False
  (<=) (ThreeKind _) _ = True
  (<=) (FullHouse _) (FourKind _) = True
  (<=) (FullHouse _) (FiveKind _) = True
  (<=) (FullHouse _) _ = False
  (<=) (FourKind _) (FiveKind _) = True
  (<=) (FourKind _) _ = False
  (<=) (FiveKind _) _ = False

type Bid = Int

data Player = Player Hand Bid deriving (Eq, Show)

instance Ord Player where
  compare (Player h _) (Player h' _) = compare h h'

parseInput :: String -> [Player]
parseInput = fmap ((\[h, b] -> Player (parseHand h) (read b)) . words) . lines
  where
    parseHand = categorize . fmap parseCard

parseCard :: Char -> Card
parseCard c =
  if c `elem` "TJQKA"
    then Face (read [c])
    else Number (read [c])

categorize :: [Card] -> Hand
categorize cs = case length groups of
  1 -> FiveKind (head cs)
  2 -> case length $ last $ sortOn length groups of -- FourKind, FullHouse
    4 -> FourKind cs
    3 -> FullHouse cs
  3 -> case length $ last $ sortOn length groups of -- ThreeKind, TwoPair
    3 -> ThreeKind cs
    2 -> TwoPair cs
  4 -> OnePair cs
  5 -> High cs
  where
    groups = group $ sort cs

solution :: [Player] -> Int
solution ps = sum $ fmap (\(r, Player _ bid) -> r * bid) ranks
  where
    ranks = zip [1 ..] (sort ps)

main = do
  ps <- parseInput <$> readInput
  print $ solution ps
