{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import Util

main =
  do
    helper "test-inputs/day-15" "Test Part 1:" $ part1 2020
    helper "inputs/day-15" "Part 1:" $ part1 2020
    helper "inputs/day-15" "Part 2:" $ part1 30000000

parseInput :: [Char] -> [Int]
parseInput = map read . splitOn ","

part1 target input =
  head states
  where
    parsed = parseInput input
    state = initialState parsed
    round = 1 + length parsed
    states = snd $ foldl' (\(state, numbers) round -> nextState (state, round) numbers) (state, reverse parsed) [round .. target]

initialState initialNums = foldl' (\state (i, n) -> Map.insert n (i, i) state) Map.empty $ zip [1 ..] initialNums

shiftIntoTuple x (a, _) = (x, a)

tupleDiff (a, b) = a - b

nextNumber :: Map.Map Int (Int, Int) -> Int -> Int
nextNumber state lastNumber
  | Map.member lastNumber state = tupleDiff (state Map.! lastNumber)
  | Map.member lastNumber state = 0

updateSeenState round (Just currentVal) = Just $ shiftIntoTuple round currentVal
updateSeenState round Nothing = Just (round, round)

nextState (state, round) x =
  (next, nextNum : x)
  where
    !lastNumber = head x
    nextNum = nextNumber state lastNumber
    next = Map.alter (updateSeenState round) nextNum state
