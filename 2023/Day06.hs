import Control.Category ((>>>))
import Data.List

-- input = "Time:      71530\n\
--         \Distance:  940200"

input = "Time:        44806572\n\
        \Distance:   208158110501102"

type Time = Int
type Distance = Int

data Race = Race Time Distance deriving (Eq, Show)

solution :: [Race] -> Int
solution = product . fmap countWays

countWays :: Race -> Int
countWays (Race duration best) =
  length $ filter (beatsBest duration best) [0..duration]

beatsBest :: Time -> Distance -> Time -> Bool
beatsBest duration best t = dist > best
  where
    dist = (duration - t) * t

parseInput :: String -> [Race]
parseInput =
  lines >>>
  fmap words >>>
  transpose >>>
  tail >>>
  fmap (\[t,d] -> Race (read t) (read d))

main = do
  let rs = parseInput input
  print rs
  print $ solution rs
