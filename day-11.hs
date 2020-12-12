{-# LANGUAGE FlexibleContexts #-}

import Data.Array
import Util

main =
  do
    helper "test-inputs/day-11" "Test Part 1:" part1
    helper "inputs/day-11" "Part 1:" part1
    helper "test-inputs/day-11" "Test Part 2" part2
    helper "inputs/day-11" "Part 2" part2

part1 = countFinalOccupiedSeats applyRulesToPos

part2 = countFinalOccupiedSeats applyRulesToPos2

data Tile = EmptyChair | FilledChair | Floor deriving (Show, Ord, Eq)

newtype Pos = Pos (Int, Int) deriving (Show, Ord, Eq, Ix)

type FloorMap = (Array Pos Tile)

parseChar 'L' = EmptyChair
parseChar '.' = Floor
parseChar '#' = FilledChair

parseInput :: [Char] -> FloorMap
parseInput input =
  array bounds idxValPair
  where
    list = map (map parseChar) (lines input)
    idxValPair = [(Pos (row, col), list !! row !! col) | row <- [0 .. (length list - 1)], col <- [0 .. (length (list !! row) - 1)]]
    bounds = (fst (head idxValPair), fst (last idxValPair))

tileAt :: FloorMap -> Pos -> Tile
tileAt floorMap pos
  | not $ inRange (bounds floorMap) pos = Floor
  | otherwise = floorMap ! pos

posAround (Pos (row, col)) =
  map
    (\(rowF, colF) -> Pos (rowF row, colF col))
    posAroundTemplate

posAroundExtended inRange pos =
  map (takeWhile inRange . tail) extendedPositions
  where
    extendedPositions = map (\(rowF, colF) -> iterate (\(Pos (row, col)) -> Pos (rowF row, colF col)) pos) posAroundTemplate

posAroundTemplate =
  -- Clockwise starting from noon
  [ (subtract 1, id),
    (subtract 1, (+ 1)),
    (id, (+ 1)),
    ((+ 1), (+ 1)),
    ((+ 1), id),
    ((+ 1), subtract 1),
    (id, subtract 1),
    (subtract 1, subtract 1)
  ]

applyRulesToPos :: FloorMap -> Pos -> Tile
applyRulesToPos floorMap pos
  | currentTile == EmptyChair && null filledChairs = FilledChair
  | currentTile == FilledChair && length filledChairs >= 4 = EmptyChair
  | otherwise = currentTile
  where
    currentTile = tileAt floorMap pos
    neighbors = map (tileAt floorMap) (posAround pos)
    filledChairs = filter (== FilledChair) neighbors

applyRulesToPos2 :: FloorMap -> Pos -> Tile
applyRulesToPos2 floorMap pos
  | currentTile == EmptyChair && filledChairs == 0 = FilledChair
  | currentTile == FilledChair && filledChairs >= 5 = EmptyChair
  | otherwise = currentTile
  where
    currentTile = tileAt floorMap pos
    neighbors = map (dropWhile ((== Floor) . tileAt floorMap)) (posAroundExtended (inRange (bounds floorMap)) pos)
    filledChairs = length $ filter (== FilledChair) (map (tileAt floorMap . head) $ filter (not . null) neighbors)

imapArray :: Ix i => (i -> e1 -> e2) -> Array i e1 -> Array i e2
imapArray f arr =
  listArray b $ map (uncurry f) l
  where
    b = bounds arr
    l = assocs arr

countFinalOccupiedSeats applyRules input = finalOccupiedSeats
  where
    floorMap = parseInput input
    iterations = iterate (\floorMap -> imapArray (\pos _ -> applyRules floorMap pos) floorMap) floorMap
    iterationsTail = tail iterations
    differentIterations = map snd $ takeWhile (uncurry (/=)) $ zip iterations iterationsTail
    finalIteration = last differentIterations
    finalOccupiedSeats = length $ filter (== FilledChair) $ elems finalIteration