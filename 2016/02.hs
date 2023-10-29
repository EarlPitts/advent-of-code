data Dir = L | R | U | D deriving (Show)
data Button = One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Show)

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

main :: IO ()
main = do
  input <- parse <$> readFile "input"
  print $ foldl step Five <$> input
