import Data.Char
import Data.Either
import Data.List
import Data.Maybe
import Text.Parsec
import Text.Parsec.String
import Utils

-- Part 1
numbers :: String -> [Int]
numbers = fmap (\c -> read [c]) . filter isNumber

edges :: [Int] -> Int
edges l = read $ concatMap show [head l, last l]

part1 :: [String] -> String
part1 = show . sum . fmap (edges . numbers)

-- Part 2
toNum "1" = 1
toNum "2" = 2
toNum "3" = 3
toNum "4" = 4
toNum "5" = 5
toNum "6" = 6
toNum "7" = 7
toNum "8" = 8
toNum "9" = 9
toNum "one" = 1
toNum "two" = 2
toNum "three" = 3
toNum "four" = 4
toNum "five" = 5
toNum "six" = 6
toNum "seven" = 7
toNum "eight" = 8
toNum "nine" = 9

smarterNumbers :: Parser [Int]
smarterNumbers = catMaybes <$> many numParser
  where
    numParser = lookAhead (optionMaybe (toNum <$> choice (try . string <$> numStrings))) <* anyChar
    numStrings = (show <$> [0 .. 9]) ++ ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

part2 :: [String] -> Int
part2 = sum . rights . fmap (fmap edges . parse smarterNumbers "")

main = solveList part2
