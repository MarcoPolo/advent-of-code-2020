import Util

main =
  do
    helper "test-inputs/day-12" "Test Part 1:" part1
    helper "inputs/day-12" "Part 1:" part1
    helper "test-inputs/day-12" "Test Part 2:" part2
    helper "inputs/day-12" "Part 2:" part2

parseInput = lines

-- (east, south)
newtype Pos = Pos (Int, Int) deriving (Show, Ord, Eq)

instance Num Pos where
  (+) (Pos (e, s)) (Pos (e1, s1)) = Pos (e + e1, s + s1)
  (*) (Pos (e, s)) (Pos (e1, s1)) = Pos (e * e1, s * s1)

scalePos (Pos (e, s)) n = Pos (e * n, s * n)

data ShipState = ShipState {pos :: Pos, direction :: Float, waypointPos :: Pos} deriving (Show)

deg2rad = (*) (pi / 180.0)

applyAction :: ShipState -> String -> ShipState
applyAction shipState (action : numStr)
  | action == 'F' = shipState {pos = pos shipState + Pos ((round . sin . deg2rad $ direction shipState) * num, (negate . round . cos . deg2rad $ direction shipState) * num)}
  | action == 'L' = shipState {direction = direction shipState - numF}
  | action == 'R' = shipState {direction = direction shipState + numF}
  | action == 'N' = shipState {pos = pos shipState + Pos (0, negate num)}
  | action == 'S' = shipState {pos = pos shipState + Pos (0, num)}
  | action == 'W' = shipState {pos = pos shipState + Pos (negate num, 0)}
  | action == 'E' = shipState {pos = pos shipState + Pos (num, 0)}
  | otherwise = shipState
  where
    num = read numStr :: Int
    numF = fromIntegral num :: Float

inc = (+) 1

rotateWayPoint :: Pos -> Int -> Pos
rotateWayPoint (Pos (e, w)) degrees
  | degrees == 0 = Pos (e, w)
  | degrees == 90 = Pos (negate w, e)
  | degrees == -90 = Pos (w, negate e)
  | degrees `mod` 90 /= 0 = error $ "Unhandled rotation " ++ show degrees
  | degrees > 0 = last $ take (inc (degrees `quot` 90)) (iterate (`rotateWayPoint` 90) (Pos (e, w)))
  | degrees < 0 = last $ take (inc (abs degrees `quot` 90)) (iterate (`rotateWayPoint` negate 90) (Pos (e, w)))
  | otherwise = error $ "Unhandled rotation " ++ show degrees

applyAction2 :: ShipState -> String -> ShipState
applyAction2 shipState (action : numStr)
  | action == 'F' = shipState {pos = pos shipState + scalePos (waypointPos shipState) num}
  | action == 'L' = shipState {waypointPos = rotateWayPoint (waypointPos shipState) (negate num)}
  | action == 'R' = shipState {waypointPos = rotateWayPoint (waypointPos shipState) num}
  | action == 'N' = shipState {waypointPos = waypointPos shipState + Pos (0, negate num)}
  | action == 'S' = shipState {waypointPos = waypointPos shipState + Pos (0, num)}
  | action == 'W' = shipState {waypointPos = waypointPos shipState + Pos (negate num, 0)}
  | action == 'E' = shipState {waypointPos = waypointPos shipState + Pos (num, 0)}
  | otherwise = shipState
  where
    num = read numStr :: Int

initialState = ShipState {pos = Pos (0, 0), direction = 90, waypointPos = Pos (10, -1)}

solve applyAction input =
  sum . map abs $ [e, s]
  where
    actions = parseInput input
    finalState = foldl applyAction initialState actions
    Pos (e, s) = pos finalState

part1 = solve applyAction

part2 = solve applyAction2