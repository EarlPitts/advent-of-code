module Day07 where

import AoC.Utils
import Data.Bifunctor
import Data.Foldable
import Data.List (isInfixOf, tails)
import Text.Parsec
import Text.Parsec.String

newtype IPv7 = IPv7 [AddressChunk] deriving (Eq, Show, Ord)

data AddressChunk = HyperNet String | SuperNet String deriving (Eq, Show, Ord)

p :: Parser [IPv7]
p = sepEndBy parseIp newline

parseIp :: Parser IPv7
parseIp = IPv7 <$> many1 (parseHyper <|> parseSuper)

parseHyper :: Parser AddressChunk
parseHyper = HyperNet <$> between (char '[') (char ']') (many1 lower)

parseSuper :: Parser AddressChunk
parseSuper = SuperNet <$> many1 lower

groupAddrChunk :: IPv7 -> ([AddressChunk], [AddressChunk])
groupAddrChunk (IPv7 is) = go is ([], [])
 where
  go [] acc = acc
  go (a@(HyperNet _) : is) (hs, ss) = go is (a : hs, ss)
  go (a@(SuperNet _) : is) (hs, ss) = go is (hs, a : ss)

isABBA :: String -> Bool
isABBA s = take 2 s == reverse (drop 2 s) && head s /= s !! 1

support :: AddressChunk -> Bool
support (HyperNet str) = not $ any isABBA (substrings 4 str)
support (SuperNet str) = any isABBA (substrings 4 str)

tls :: IPv7 -> Bool
tls ip = all support hs && any support ss
 where
  (hs, ss) = groupAddrChunk ip

substrings :: Int -> String -> [String]
substrings n str
  | length str < n = []
  | otherwise = take n str : substrings n (tail str)

ssl :: IPv7 -> Bool
ssl ip = any (`hasBAB` concatMap getABAs ss) hs
 where
  (hs, ss) =
    bimap
      (fmap (\(HyperNet str) -> str))
      (fmap (\(SuperNet str) -> str))
      (groupAddrChunk ip)

getABAs :: String -> [String]
getABAs = filter isABA . substrings 3

abaToBab :: String -> String
abaToBab [a, b, _] = [b, a, b]

isABA :: String -> Bool
isABA = \case
  [a, b, c] -> a == c && a /= b
  _ -> False

hasBAB :: String -> [String] -> Bool
hasBAB chunk = any $ (`isInfixOf` chunk) . abaToBab

main :: IO ()
main = do
  Right ips <- parse p "" <$> readInput
  print $ length $ filter tls ips
  print $ length $ filter ssl ips
