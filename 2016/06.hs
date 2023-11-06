import Data.List

mostFreq = head . last . sortOn length . group . sort

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  print $ mostFreq <$> transpose input
