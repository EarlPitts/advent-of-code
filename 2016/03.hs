parse :: String -> [(Int,Int,Int)]
parse = fmap f . lines
  where
    f = g . fmap read . words
    g [x,y,z] = (x,y,z)

validate :: (Int,Int,Int) -> Bool
validate (x,y,z) = x + y > z && x + z > y && y + z > x

main :: IO ()
main = do
  input <- parse <$> readFile "input"
  print $ length (filter validate input)
