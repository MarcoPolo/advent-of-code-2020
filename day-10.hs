{-# LANGUAGE FlexibleContexts #-}

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

main =
  do
    testContent <- readFile "test-inputs/day-10"
    testContent2 <- readFile "test-inputs/day-10-1"
    content <- readFile "inputs/day-10"
    putStr "Test Part 1: "
    print $ show (part1 testContent)
    putStr "Test 2 Part 1: "
    print $ show (part1 testContent2)
    putStr "Part 1: "
    print $ show (part1 content)
    putStr "Test Part 2: "
    print $ show (part2 testContent)
    putStr "Test 2 Part 2: "
    print $ show (part2 testContent2)
    putStr "Part 2: "
    print $ show (part2 content)

parseInput :: String -> [Int]
parseInput = map read . lines

part1 input =
  distribution Map.! negate 1 * distribution Map.! negate 3
  where
    sorted = sort $ parseInput input
    firstDiff = negate $ head sorted
    diffs = firstDiff : zipWith (-) sorted (tail sorted) ++ [-3]
    distribution = foldl' (\acc diff -> Map.insert diff (Map.findWithDefault 0 diff acc + 1) acc) Map.empty diffs :: Map.Map Int Int

part2 input =
  product $ map (length . combinations) splitDiffs
  where
    sorted = sort $ parseInput input
    firstDiff = head sorted
    diffs = firstDiff : zipWith (-) (tail sorted) sorted ++ [3]
    splitDiffs = filter (not . null) $ splitWhen (== 3) diffs

combinations :: [Int] -> [[Int]]
combinations [x] = [[x]]
combinations [1, 1] = [[1, 1], [2]]
combinations [2, 1] = [[2, 1], [3]]
combinations [1, 2] = [[1, 2], [3]]
combinations list = dedup $ list : concatMap combinations headPart ++ concatMap combinations lastPart
  where
    dedup = Set.toList . Set.fromList -- Hack!
    -- The first item here is the initial combination, so we can get rid of it
    (_ : headPart) = dedup $ map (\x -> head list : x) (combinations (tail list))
    (_ : lastPart) = dedup $ map (\x -> x ++ [last list]) (combinations (init list))