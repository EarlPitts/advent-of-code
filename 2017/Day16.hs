module Day16 where

import Data.List
import Text.Parsec
import Text.Parsec.String

type Program = Char
type Index   = Int

data Instruction
  = Spin Index
  | Exchange Index Index
  | Partner Program Program
  deriving (Show,Eq)

p :: Parser [Instruction]
p = sepBy (pSpin <|> pExchange <|> pPartner) (char ',')

pSpin :: Parser Instruction
pSpin = do
  char 's'
  n <- read <$> many1 digit
  return $ Spin n

pExchange :: Parser Instruction
pExchange = do
  char 'x'
  n1 <- read <$> many1 digit
  char '/'
  n2 <- read <$> many1 digit
  return $ Exchange n1 n2

pPartner :: Parser Instruction
pPartner = do
  char 'p'
  p1 <- lower
  char '/'
  p2 <- lower
  return $ Partner p1 p2

spin :: Index -> [Program] -> [Program]
spin n ps = y ++ x
  where
    (x, y) = splitAt (length ps - n) ps

exchange :: Index -> Index -> [Program] -> [Program]
exchange n1 n2 ps = beg ++ [p2] ++ mid ++ [p1] ++ end
  where
    beg = take (min n1 n2) ps
    mid = take (snd - fst - 1) (drop (fst + 1) ps)
    end = drop (snd + 1) ps
    p1 = ps !! fst
    p2 = ps !! snd
    fst = min n1 n2
    snd = max n1 n2

partner :: Program -> Program -> [Program] -> [Program]
partner p1 p2 ps = exchange i1 i2 ps
  where
    (Just i1) = elemIndex p1 ps
    (Just i2) = elemIndex p2 ps

evalStep :: Instruction -> [Program] -> [Program]
evalStep (Spin n)         = spin n
evalStep (Exchange n1 n2) = exchange n1 n2
evalStep (Partner p1 p2)  = partner p1 p2

evalDance :: [Instruction] -> [Program] -> [Program]
evalDance is ps = foldl (flip evalStep) ps is

main :: IO ()
main = do
  input <- getContents
  let (Right is) = parse p "" input
  print (evalDance is ['a'..'p'])
