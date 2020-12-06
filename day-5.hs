import Data.Char (digitToInt)
import Data.List
import Data.Maybe (fromJust, listToMaybe)
import Numeric (readInt)

-- readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

parseInput (x : xs)
  | x == 'B' = ('1' : parseInput xs)
  | x == 'B' = ('1' : parseInput xs)
  | x == 'F' = ('0' : parseInput xs)
  | x == 'R' = ('1' : parseInput xs)
  | x == 'L' = ('0' : parseInput xs)
  | otherwise = error "Unexpected input " ++ show x
parseInput [] = []

main =
  do
    testContent <- readFile "inputs/day-5"
    putStr "Part 1: "
    print (part1 testContent)
    putStr "Part 2: "
    print (part2 testContent)

part1 content =
  let parsed = map parseInput (lines content)
   in readBin (maximum parsed)

part2 content =
  let parsed = sort (map (fromJust . readBin . parseInput) (lines content)) :: [Int]
   in findSeat parsed

findSeat :: [Int] -> Int
findSeat (x : y : xs)
  | x + 1 == y = findSeat (y : xs)
  | otherwise = x + 1