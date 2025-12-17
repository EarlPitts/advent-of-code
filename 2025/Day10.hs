module Day10 where

import AoC.Utils
import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.String

data Machine = Machine [Light] [Button] Joltage deriving (Show, Eq)

data Light = On | Off deriving (Show, Eq)

type Idx = Int

type Button = [Idx]

type Joltage = [Int]

p :: Parser [Machine]
p = sepEndBy (Machine <$> pLights <*> pButtons <*> pJoltage) newline

pLights :: Parser [Light]
pLights = do
  char '['
  lights <- many1 $ choice [char '.', char '#']
  string "] "
  pure $ fmap (\c -> if c == '.' then Off else On) lights

pButtons :: Parser [Button]
pButtons = sepEndBy pButton space

pButton :: Parser Button
pButton = char '(' *> sepBy int (char ',') <* char ')'

pJoltage :: Parser Joltage
pJoltage = char '{' *> sepBy int (char ',') <* char '}'

pressNum :: Machine -> Int
pressNum m@(Machine ls bs _) = head $ sort $ fmap sum $ filter (valid m) $ generate (length bs)

generate :: Int -> [[Int]]
generate n = replicateM n [0, 1]

valid :: Machine -> [Int] -> Bool
valid (Machine ls bs _) ps = all f (zip [0 ..] ls)
  where
    f (idx, light) = case M.lookup idx presses of
      Nothing -> light == Off
      Just num -> if light == On then odd num else even num
    presses = M.fromList $ fmap (\l -> (head l, length l)) $ group $ sort $ concat $ fmap concat $ replicate <$> (ZipList ps) <*> (ZipList bs)


main :: IO ()
main = do
  -- Right ps <- parse p "" <$> readInput
  Right ps <- parse p "" <$> readExample
  -- void $ traverse (print . pressNum) ps
  print $ sum $ fmap pressNum ps

-- print ps

-- print $ solution ps
