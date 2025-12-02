module Day01 where

import AoC.Utils

data Rotation = L Int | R Int deriving (Show, Eq)

parse :: String -> [Rotation]
parse = fmap f . lines
  where
    f line = case head line of
      'L' -> L (read (tail line))
      'R' -> R (read (tail line))

solution :: [Rotation] -> Int
solution = fst . foldl f (0, 50)
  where
    f (c, dial) = \case
      (L n) ->
        let newDial = mod (dial - n) 100
         in if newDial == 0 then (c + 1, newDial) else (c, newDial)
      (R n) ->
        let newDial = mod (dial + n) 100
         in if newDial == 0 then (c + 1, newDial) else (c, newDial)

solution' :: [Rotation] -> Int
solution' = fst . foldl f (0, 50)
  where
    f (c, dial) = \case
      (R n) ->
        let (clicks, newDial) = divMod (dial - n) 100
         in (c + (abs clicks) + if dial == 0 then (-1) else 0 + if newDial == 0 then 1 else 0, newDial)
      (L n) ->
        let (clicks, newDial) = divMod (dial + n) 100
         in (c + (abs clicks), newDial)

main :: IO ()
main = do
  input <- parse <$> readInput
  putStrLn "Part 1"
  print $ solution input
  putStrLn "Part 2"
  print $ solution' input
