import Data.Char

main = interact solution'

solution :: String -> String
solution = show . sum . map f . lines
  where f = checksum . map read . words

checksum :: [Int] -> Int
checksum nums = maxim - minim
  where maxim = maximum nums
        minim = minimum nums

solution' = show . sum . map f . lines
  where f = uncurry div . head . filter (uncurry evenDiv) . addFlip . pairs . map read . words
        addFlip p = p ++ map (\(x,y) -> (y,x)) p

pairs :: [Int] -> [(Int, Int)]
pairs (n:ns) = go n ns
  where go _ [] = []
        go h t@(n:ns) = map (h,) t ++ go n ns
        
evenDiv :: Int -> Int -> Bool
evenDiv n m = mod n m == 0
