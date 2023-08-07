import Control.Applicative
import Data.List.Split

step :: Int -> Int -> [Int] -> [Int]
step len skip l = beg ++ end
  where
    changed = take len (cycle l)
    unchanged = drop len l
    end = take skip (unchanged ++ reverse changed)
    beg = drop skip (unchanged ++ reverse changed)

run :: [Int] -> [Int] -> [Int]
run ls = foldl (.) id (reverse (getZipList fs))
  where fs = liftA2 step (ZipList ls) (ZipList [0..])

solve :: [Int] -> Int
solve ls = product $ take 2 $ take (length res) $ drop offset $ cycle res 
  where
    offset = length res - mod (sum ls + sum [0..length ls-1]) (length res) 
    res = run ls [0..255]

main :: IO ()
main = do
  input <- getContents
  let lengths = read <$> splitOn "," input
  print (solve lengths)
