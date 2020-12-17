import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Util

main =
  do
    helper "test-inputs/day-17" "Test Part 1:" part1
    helper "inputs/day-17" "Part 1:" part1
    helper "test-inputs/day-17" "Test Part 2:" part2
    helper "inputs/day-17" "Part 2:" part2

part1 input = length $ filter (== Cube) $ Map.elems finalState
  where
    initialState = parseInput input
    finalState = iterate' (nextState neighborPos) initialState !! 6

part2 input = length $ filter (== Cube) $ Map.elems finalState
  where
    initialState = parseInput2 input
    finalState = iterate' (nextState neighborPos2) initialState !! 6

data Cell = Cube | Empty deriving (Show, Ord, Eq)

-- Not used, but these are what the types look like for part1. Part 2 is the
-- same but uses 4 coordinates (i.e. Pos is (Int, Int, Int, Int))
-- type Pos = (Int, Int, Int)
-- type State = Map.Map (Int, Int, Int) Cell

neighborRange_ = [-1 .. 1]

neighborDiffs :: [(Int, Int, Int)]
neighborDiffs = [(x, y, z) | x <- neighborRange_, y <- neighborRange_, z <- neighborRange_, (x, y, z) /= (0, 0, 0)]

neighborDiffs2 :: [(Int, Int, Int, Int)]
neighborDiffs2 = [(x, y, z, w) | x <- neighborRange_, y <- neighborRange_, z <- neighborRange_, w <- neighborRange_, (x, y, z, w) /= (0, 0, 0, 0)]

neighborPos (x, y, z) = map (\(dx, dy, dz) -> (x + dx, y + dy, z + dz)) neighborDiffs

neighborPos2 (x, y, z, w) = map (\(dx, dy, dz, dw) -> (x + dx, y + dy, z + dz, w + dw)) neighborDiffs2

positionsToCheck state neighborPos = Set.elems $ foldl1 Set.union $ map (Set.fromList . neighborPos) (Map.keys state)

nextState neighborPos state =
  Map.fromList $ map (\cellPos -> (cellPos, nextStateForCell neighborPos state cellPos (Map.lookup cellPos state))) toCheck
  where
    toCheck = positionsToCheck state neighborPos

nextStateForCell neighborPos state cellPos cell
  | cell == Just Cube && activeNeighbors `elem` [2, 3] = Cube
  | (cell == Just Empty || isNothing cell) && activeNeighbors == 3 = Cube
  | otherwise = Empty
  where
    neighbors = map (`Map.lookup` state) (neighborPos cellPos)
    activeNeighbors = length $ filter (== Just Cube) neighbors

parseInput input =
  Map.fromList [((x, y, 0), parseCell ((ls !! y) !! x)) | y <- [0 .. length ls - 1], x <- [0 .. length (ls !! y) - 1]]
  where
    ls = lines input

parseInput2 input =
  Map.fromList [((x, y, 0, 0), parseCell ((ls !! y) !! x)) | y <- [0 .. length ls - 1], x <- [0 .. length (ls !! y) - 1]]
  where
    ls = lines input

parseCell '#' = Cube
parseCell _ = Empty