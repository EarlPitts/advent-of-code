import Data.Tree as T
import Text.Parsec
import Text.Parsec.String

data GroupInfo
  = Group
  | Garbage Int
  deriving (Show, Eq)

groupParser :: Parser (T.Tree GroupInfo)
groupParser = do
  char '{'
  groups <- sepBy (groupParser <|> garbageParser) (char ',')
  char '}'
  return (T.Node Group groups)

garbageAccum :: Char -> String -> String
garbageAccum '!' = id
garbageAccum c = (:) c

garbageParser :: Parser (Tree GroupInfo)
garbageParser = do
  char '<'
  gs <- garbage
  char '>'
  return (T.Node (Garbage (length gs)) [])
  where
    garbage = manyAccum garbageAccum garbageChar
    garbageChar = choice . map try $ [char '!' <* anyChar, noneOf ">"]

solve :: T.Tree GroupInfo -> Int
solve t = sum $ zipWith (*) [1..] (length <$> fmap (filter ((==) Group)) (levels t))

solve' :: T.Tree GroupInfo -> Int
solve' = sum . fmap len . filter isGarbage . flatten
  where
    isGarbage (Garbage _) = True
    isGarbage _           = False
    len (Garbage n) = n

main :: IO ()
main = do
  input <- getContents
  let (Right t) = parse groupParser "" input
  print (solve t)
  print (solve' t)
