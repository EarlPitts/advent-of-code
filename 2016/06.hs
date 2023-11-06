import Data.List

freq :: ([String] -> String) -> String -> Char
freq f = head . f . sortOn length . group . sort

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  print $ freq last <$> transpose input
  print $ freq head <$> transpose input
