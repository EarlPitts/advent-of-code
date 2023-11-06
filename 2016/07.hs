import Text.Parsec
import Text.Parsec.String

newtype IPv7 = IPv7 [AddressChunk] deriving (Eq, Show, Ord)

data AddressChunk = HyperNet String | NonHyperNet String deriving (Eq, Show, Ord)

p :: Parser [IPv7]
p = sepEndBy parseIp newline

parseIp :: Parser IPv7
parseIp = IPv7 <$> many1 (parseHyper <|> parseNonHyper)

parseHyper :: Parser AddressChunk
parseHyper = HyperNet <$> between (char '[') (char ']') (many1 lower)

parseNonHyper :: Parser AddressChunk
parseNonHyper = NonHyperNet <$> many1 lower

groupAddrChunk :: IPv7 -> ([AddressChunk],[AddressChunk])
groupAddrChunk (IPv7 is) = go is ([],[])
  where
    go [] acc = acc
    go (a@(HyperNet _):is) (hs,nhs) = go is (a:hs,nhs)
    go (a@(NonHyperNet _):is) (hs,nhs) = go is (hs,a:nhs)

isABBA :: String -> Bool
isABBA s = take 2 s == reverse (drop 2 s) && head s /= s !! 1

support :: AddressChunk -> Bool
support (HyperNet str) = not $ any isABBA (substrings 4 str)
support (NonHyperNet str) = any isABBA (substrings 4 str)

tls :: IPv7 -> Bool
tls ip = all support hs && any support nhs
  where
    (hs,nhs) = groupAddrChunk ip

substrings :: Int -> String -> [String]
substrings n str
  | length str < n = []
  | otherwise = take n str:substrings n (tail str)

main :: IO ()
main = do
  input <- readFile "input"
  let Right ips = parse p "" input
  print $ length $ filter tls ips
