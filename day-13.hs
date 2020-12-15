import Control.Parallel.Strategies
import qualified Data.Bifunctor
import Data.List
import Data.List.Split
import Data.Maybe
import Util

main =
  do
    helper "test-inputs/day-13" "Test Part 1:" part1
    helper "inputs/day-13" "Part 1:" part1

    -- helper "test-inputs/day-13" "Test Part 2:" part2

    -- helper "test-inputs/day-13-2" "Test Part 2–2:" part2
    -- helper "test-inputs/day-13" "Test Scratch Part 2:" part2Scratch
    helper "test-inputs/day-13-1" "Test Scratch Part 2–1:" part2Scratch
    helper "test-inputs/day-13-2" "Test Scratch Part 2–2:" part2Scratch
    helper "test-inputs/day-13" "Test Scratch Part 2:" part2
    helper "inputs/day-13" "Part 2:" part2

-- helper "test-inputs/day-13-2" "Test Part 2:" part2

-- helper "inputs/day-13" "Part 2:" part2

parseInput input =
  (read departTime, busIDs)
  where
    [departTime, busIDs_] = lines input
    busIDs = map read $ filter (/= "x") $ splitOn "," busIDs_ :: [Int]

part1 input =
  wait * busID
  where
    (departTime, busIDs) = parseInput input
    waits = map (\id -> id - (departTime `mod` id)) busIDs
    (wait, busID) = minimum $ zip waits busIDs

data BusID = BusID Int | NoConstraint deriving (Show, Eq, Ord)

idOfBus (BusID x) = x
idOfBus NoConstraint = error "Can't get id of no constaint"

parseBusID :: String -> BusID
parseBusID "x" = NoConstraint
parseBusID id = BusID $ read id

parseInput2 input =
  busIDs
  where
    (departTime : busIDs_ : _) = lines input
    busIDs = map parseBusID $ splitOn "," busIDs_

part2Scratch input =
  ( take 4 startTimes,
    busIDsOnly,
    part2 input,
    busIDs,
    (map (\n -> ((3 * n) - 0)) [0 .. 5]),
    (map (\n -> ((5 * n) - 1)) [0 .. 12]),
    filter ((== 0) . (`mod` 3)) $ (map (\n -> ((7 * n) - 2)) [0 .. 10]),
    filter ((== 0) . (`mod` 5)) $ (map (\n -> ((7 * n) - 1)) [0 .. 10]),
    startTimesTest
    -- multipliers,
    -- maxMultipler
  )
  where
    -- take 5 downNumss

    -- maxBusID

    -- take 10 $ candidateNums

    busIDs = parseInput2 input
    busIDsOnly = filter ((/= NoConstraint) . snd) $ zip [0 ..] busIDs
    validators = map (uncurry isNumValid) busIDsOnly
    validators0 = map (isNumValid 0 . snd) busIDsOnly
    (maxIdx, maxBusID) = maximumBy (\(_, a) (_, b) -> compare a b) busIDsOnly
    -- candidateNums = concat $ transpose $ zipWith candidates [0 ..] (filter
    -- (/= NoConstraint) busIDs)
    maxProduct = product $ map ((\(BusID x) -> x) . snd) busIDsOnly
    maxMultipler = maxProduct `div` (idOfBus maxBusID)
    -- candidateNums = candidatesDownFrom maxIdx maxBusID maxMultipler
    candidateNums = candidates maxIdx maxBusID
    upNums = transpose $ map (\(fstBus, (idx, busID)) -> upFrom idx fstBus busID) $ zip (repeat (head busIDs)) busIDsOnly
    downNumss = transpose $ map (\(fstBus, (idx, busID)) -> downFrom idx fstBus busID) $ zip (repeat (head busIDs)) busIDsOnly
    startTimes = filter (\num -> all (\f -> f num) validators) candidateNums
    startTimes0 = filter (\num -> all (\f -> f num) validators0) candidateNums
    startTimesTest = filter (\num -> all (\f -> f num) validators) [0]
    multipliers = map (\(idx, (BusID i)) -> (head startTimes + idx) `div` i) busIDsOnly

part2 input =
  -- (startTime, isValidStartTime startTime busIDs)
  (startTime, head $ dropWhile (< 0) $ iterate (+ prod) startTime)
  where
    busIDs = parseInput2 input
    -- shiftedBusIDs = iterate shiftBusIDs busIDs
    busIDsOnly = filter ((/= NoConstraint) . snd) $ zip [0 ..] busIDs
    aAndNs = map (Data.Bifunctor.first negate . Data.Bifunctor.second (fromIntegral . idOfBus)) busIDsOnly
    prod = product (map snd aAndNs)
    startTime = chineseremainder (map fst aAndNs) (map snd aAndNs)

-- (maxIdx, maxBusID) = maximumBy (\(_, a) (_, b) -> compare a b) busIDsOnly
-- candidateNums = candidates maxIdx maxBusID

shiftBusIDs = (NoConstraint :)

isValidStartTime startTime busIDs =
  all (\f -> f startTime) validators
  where
    validators = zipWith isNumValid [0 ..] busIDs

isNumValid :: Int -> BusID -> Int -> Bool
isNumValid _ NoConstraint _ = True
isNumValid busIdx (BusID busID) num = (num + busIdx) `mod` busID == 0

candidates busIdx (BusID busID) = map (\n -> (busID * n) - busIdx) [1 ..]

candidatesDownFrom busIdx (BusID busID) number = map (\n -> (busID * n) - busIdx) [number, (number - 1) .. 0]

upFrom busIdx (BusID firstBus) (BusID busID) = map (\n -> ((busID * firstBus) + busID * n) `mod` firstBus) [1 ..]

downFrom busIdx (BusID firstBus) (BusID busID) = map (\n -> ((busID * firstBus) - busID * n) `mod` firstBus) [1 ..]

-- binarySearchMatch target (BusID firstBus) busIdx (BusID busID) multiplier
--   | currentNum >
--   where currentNum = ((busID * multiplier) - busIdx) `mod` firstBus

-- id0 * r == id1 * s - 1 == id2 * t - 2
-- num == id0 * r == id1 * s - 1
-- r == (id1 / id0) * s - 1 / id0

-- num == id0 * r == id1 * s - 1
{-

num == id0 * r == id1 * s - 1
num == 9
id0 = 3
id1 = 4
r = 3
s = 2

9 == id

(-1 + (5 * s)) % 3 == 0
(-1 + (5 * s)) % 3 == 0
(-1 / s) % 3 + 2 == 0

(5s - 1) % 3 == 0
(5s%3 - 1 % 3) % 3 == 0
5s%3 ==  1 % 3

5 * 1, 2, 3,
    2, 1, 0

5 * 3 = 15

15 % 3 == 0

20 % 3

(5s^2 - 1s) % 3 == 0

(5s+9)(s−2) == 0

((7 * 8) - 2) == 54

()

-}

extended_gcd :: Integer -> Integer -> [Integer]
extended_gcd a b
  | mod a b == 0 = [0, 1, b]
  | otherwise = [y, x - y * (div a b), z]
  where
    [x, y, z] = extended_gcd b (mod a b)

chineseremainder :: [Integer] -> [Integer] -> Integer
chineseremainder as ns =
  let prod = product ns
      ls = [extended_gcd x (div prod x) !! 1 | x <- ns]
   in sum [div (x * y * prod) z | (x, y, z) <- zip3 as ls ns]