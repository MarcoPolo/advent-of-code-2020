{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

import Data.Char (digitToInt)
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, listToMaybe)
import Numeric (readInt)

main =
  do
    testContent <- readFile "test-inputs/day-6"
    content <- readFile "inputs/day-6"
    putStr "Test Part 1: "
    print (part1 testContent)
    putStr "Part 1: "
    print (part1 content)
    putStr "Test Part 2: "
    print (part2 testContent)
    putStr "Part 2: "
    print (part2 content)

part1 input =
  let groups = map (Map.fromList . map (,1) . filter (/= "\n") . filter (/= "") . splitOn "") (splitOn "\n\n" input)
   in sum $ map length groups

part2 input =
  let groups = map (map (Map.fromList . map (,1)) . splitOn "\n") (splitOn "\n\n" input)
      commonAnswers = map (foldl1' Map.intersection) groups
   in --
      sum $ map length commonAnswers