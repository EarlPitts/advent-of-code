import Control.Comonad
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace
import Grid
import Utils

input =
  "467..114..\n\
  \...*......\n\
  \..35..633.\n\
  \......#...\n\
  \617*......\n\
  \.....+.58.\n\
  \..592.....\n\
  \......755.\n\
  \...$.*....\n\
  \.664.598.."

solution1 = solveList $ sum . join . join . toLists . extend nums . fromLists

grid :: Grid Char
grid = fromLists $ words input

nums :: Grid Char -> [Int]
nums g = do
  guard $ isSpecial (focus g)
  guard $ nextToNumber g
  nub $ mapMaybe readNum (adjacent g)

nextToNumber :: Grid Char -> Bool
nextToNumber = any (isNumber . focus) . adjacent

isSpecial :: Char -> Bool
isSpecial c = not (isNumber c || c == '.')

readNum :: Grid Char -> Maybe Int
readNum g = do
  guard $ isNumber (focus g)
  let start = rewind g
  return $ getNum start

rewind :: Grid Char -> Grid Char
rewind g = case safeLeft g of
  Nothing -> g
  Just c -> if isNumber (focus c) then rewind (moveLeft g) else g

getNum :: Grid Char -> Int
getNum = read . mapMaybe (fmap focus) . takeWhile check . iterate (>>= safeRight) . Just
  where
    check = maybe False (isNumber . focus)
