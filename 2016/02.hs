data Dir = L | R | U | D deriving (Show)
data Button = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | A | B | C | D' deriving (Show)

parse :: String -> [[Dir]]
parse = (fmap . fmap) f . lines
  where
    f 'U' = U
    f 'D' = D
    f 'L' = L
    f 'R' = R

-- I'm very proud of this
step :: Button -> Dir -> Button
step One R    = Two
step One D    = Four
step Two L    = One
step Two D    = Five
step Two R    = Three
step Three L  = Two
step Three D  = Six
step Four U   = One
step Four R   = Five
step Four D   = Seven
step Five U   = Two
step Five D   = Eight
step Five L   = Four
step Five R   = Six
step Six U    = Three
step Six D    = Nine
step Six L    = Five
step Seven U  = Four
step Seven R  = Eight
step Eight U  = Five
step Eight L  = Seven
step Eight R  = Nine
step Nine U   = Six
step Nine L   = Eight
step b    _   = b

--     1
--   2 3 4
-- 5 6 7 8 9
--   A B C
--     D
--

-- I'm even prouder of this
step' :: Button -> Dir -> Button
step' One D   = Three
step' Two D   = Six
step' Two R   = Three
step' Three U = One
step' Three D = Seven
step' Three L = Two
step' Three R = Four
step' Four L  = Three
step' Four D  = Eight
step' Five R  = Six
step' Six U   = Two
step' Six D   = A
step' Six L   = Five
step' Six R   = Seven
step' Seven U = Three
step' Seven D = B
step' Seven L = Six
step' Seven R = Eight
step' Eight U = Four
step' Eight D = C
step' Eight L = Seven
step' Eight R = Nine
step' Nine L  = Eight
step' A U     = Six
step' A R     = B
step' B U     = Seven
step' B L     = A
step' B D     = D'
step' B R     = C
step' C U     = Eight
step' C L     = B
step' D' U    = B
step' b _     = b

main :: IO ()
main = do
  input <- parse <$> readFile "input"
  print $ foldl step Five <$> input
  print $ foldl step' Five <$> input
