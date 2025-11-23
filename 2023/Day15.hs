module Day15 where

import AoC.Utils
import Control.Monad.State
import Data.Char
import qualified Data.List as L
import Data.List.Split
import qualified Data.Map as M

type Label = String

type Length = Int

data Step = Put Label Length | Remove Label deriving (Show, Eq)

type HashMap = M.Map Int [(Label, Length)]

hash :: String -> Int
hash = foldl hashSingle 0

hashSingle :: Int -> Char -> Int
hashSingle curr c = mod ((curr + ord c) * 17) 256

solution :: String -> Int
solution = sum . (fmap hash) . splitOn ","

parse :: String -> [Step]
parse = fmap parseStep . splitOn ","

parseStep :: String -> Step
parseStep str = if op == '=' then Put label len else Remove label
  where
    label = takeWhile (\c -> not (c == '=' || c == '-')) str
    op = head $ drop (length label) str
    len = read $ drop ((length label) + 1) str

hashMap :: [Step] -> HashMap
hashMap = foldl f M.empty
  where
    f m (Put label len) = case M.lookup ind m of
      Nothing -> M.insert ind [(label, len)] m
      Just box -> case L.find (\(label', _) -> label' == label) box of
        Nothing -> M.insert ind (box ++ [(label, len)]) m
        Just _ -> M.insert ind (fmap (\orig@(label', _) -> if label' == label then (label, len) else orig) box) m
      where
        ind = hash label
    f m (Remove label) = case M.lookup ind m of
      Nothing -> m
      Just box -> M.insert ind (L.filter (\(l, _) -> l /= label) box) m
      where
        ind = hash label

power :: Int -> [(Label, Length)] -> Int
power box ls = sum $ f <$> (zip [1 ..] lengths)
  where
    lengths = snd <$> ls
    f (ind, len) = (box + 1) * ind * len

solution' :: [Step] -> Int
solution' steps = sum $ uncurry power <$> M.toList (hashMap steps)

main = do
  input <- (filter (not . (==) '\n')) <$> readInput
  let steps = parse input
  print $ solution' steps
