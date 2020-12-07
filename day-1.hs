import Control.Monad (join)
import Data.List
import Data.Maybe

targetNum = 2020

main = do
  content <- readFile "inputs/day-1"
  let numbers = sort $ map read $ lines content :: [Int]
      revNumbers = reverse numbers
      magicPair = findMatchingNumber numbers revNumbers
      magicPair2 = fromMaybe (0, 0, 0) $ findMatchingNumberPart2 numbers
   in putStrLn $ printAnswer magicPair ++ "\n\n" ++ printAnswer2 magicPair2

printAnswer (a, b) = "Part 1: Magic Pair: " ++ show (a, b) ++ "\n" ++ "Answer: " ++ show (a * b)

printAnswer2 (a, b, c) = "Part 2: Magic Pair: " ++ show (a, b, c) ++ "\n" ++ "Answer: " ++ show (a * b * c)

findMatchingNumber :: [Int] -> [Int] -> (Int, Int)
findMatchingNumber (smallNumber : numbers) (bigNumber : revNumbers)
  | smallNumber + bigNumber == targetNum = (smallNumber, bigNumber)
  | smallNumber + bigNumber < targetNum = findMatchingNumber numbers (bigNumber : revNumbers)
  | smallNumber + bigNumber > targetNum = findMatchingNumber (smallNumber : numbers) revNumbers

findMatchingNumberPart2 :: [Int] -> Maybe (Int, Int, Int)
findMatchingNumberPart2 numbers =
  let candidateNumbers = filter (\(x, y) -> x + y < targetNum) [(x, y) | x <- numbers, y <- numbers]
   in do
        (matchA, matchB) <- find (isMatch numbers) candidateNumbers
        Just (matchA, matchB, targetNum - (matchA + matchB))

isMatch :: [Int] -> (Int, Int) -> Bool
isMatch numbers (a, b) = isJust $ find ((targetNum - (a + b)) ==) numbers
