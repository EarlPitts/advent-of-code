parse :: String -> [(Int,Int,Int)]
parse = fmap f . lines
  where
    f = g . fmap read . words
    g [x,y,z] = (x,y,z)

validate :: (Int,Int,Int) -> Bool
validate (x,y,z) = x + y > z && x + z > y && y + z > x

groupByNum :: Int -> [a] -> [[a]]
groupByNum _ [] = []
groupByNum n xs = take n xs : groupByNum n (drop n xs)

tuplify :: [Int] -> (Int,Int,Int)
tuplify [a,b,c] = (a,b,c)

main :: IO ()
main = do
  input <- parse <$> readFile "input"
  print $ length (filter validate input)
  let (a,b,c) = unzip3 input
  print $ length (filter validate (tuplify <$> groupByNum 3 (a ++ b ++ c)))
