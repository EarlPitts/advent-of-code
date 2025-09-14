{-# LANGUAGE RankNTypes #-}

module Day08 where

import Data.List
import qualified Data.Map as M
import Debug.Trace
import Text.Parsec
import Text.Parsec.String
import Utils

-- input =
--   "LLR\n\n\
--   \AAA = (BBB, BBB)\n\
--   \BBB = (AAA, ZZZ)\n\
--   \ZZZ = (ZZZ, ZZZ)"

input =
  "LR\n\n\
  \11A = (11B, XXX)\n\
  \11B = (XXX, 11Z)\n\
  \11Z = (11B, XXX)\n\
  \22A = (22B, XXX)\n\
  \22B = (22C, 22C)\n\
  \22C = (22Z, 22Z)\n\
  \22Z = (22B, 22B)\n\
  \XXX = (XXX, XXX)"

data Dir = L | R deriving (Show, Eq, Read)

newtype Node = Node String deriving (Show, Eq, Ord)

data Connected = Connected {left :: Node, right :: Node} deriving (Show, Eq)

newtype Network = Network {getNetwork :: M.Map Node Connected} deriving (Show, Eq)

p :: Parser ([Dir], Network)
p = (,) <$> (pDirs <* newline <* newline) <*> pNetwork

pDirs :: Parser [Dir]
pDirs = many $ (read <$> string "R") <|> (read <$> string "L")

pNetwork :: Parser Network
pNetwork = Network . M.fromList <$> sepEndBy pNode newline

pNode :: Parser (Node, Connected)
pNode = do
  name <- many alphaNum
  space >> char '=' >> space >> char '('
  left <- many alphaNum
  char ',' >> space
  right <- many alphaNum
  char ')'
  return (Node name, Connected (Node left) (Node right))

walk :: [Dir] -> Network -> Node -> [Node]
walk ds (Network n) start = scanl next start (cycle ds)
  where
    next curr dir = case dir of
      L -> maybe (error "") left (M.lookup curr n)
      R -> maybe (error "") right (M.lookup curr n)

solution :: [Dir] -> Network -> Int
solution ds n =
  length $ takeWhile (\(Node name) -> name /= "ZZZ") $ walk ds n (Node "AAA")

startPoints :: Network -> [Node]
startPoints (Network n) = filter (\(Node name) -> last name == 'A') (M.keys n)

-- solution' :: [Dir] -> Network -> Int
-- solution' ds n = length $ takeWhile (not . all endPoint) paths
--   where
--     paths = transpose (walk ds n <$> startPoints n)

solution' :: [Dir] -> Network -> Int
solution' ds n = foldr1 lcm cycles
  where
    cycles = cycleLength . walk ds n <$> startPoints n

endPoint :: Node -> Bool
endPoint (Node name) = last name == 'Z'

cycleLength :: [Node] -> Int
cycleLength = length . takeWhile (not . endPoint)

main :: IO ()
main = do
  input <- readInput
  let Right (dirs, network) = parse p "" input
  -- print $ solution' (cycle dirs) network
  print $ solution dirs network
  print $ solution' (cycle dirs) network
