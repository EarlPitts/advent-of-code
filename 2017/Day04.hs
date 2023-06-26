import Data.List

solution :: String -> String
solution = show . length . filter validate . lines

validate :: String -> Bool
validate line = length (nub ws) == length ws
  where ws = words line

solution' :: String -> String
solution' = show . length . filter validate' . lines

validate' :: String -> Bool
validate' line = length (nub (sort <$> ws)) == length ws
  where ws = words line
