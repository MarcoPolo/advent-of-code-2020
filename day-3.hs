{-# LANGUAGE NamedFieldPuns #-}

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Debug.Trace

main = do
  content <- readFile "inputs/day-3"
  let txtMap = map cycle $ lines content
   in putStrLn $
        "Part 1: " ++ show (part1 txtMap Position {x = 0, y = 0} Velocity {vx = 3, vy = 1}) ++ "\n"
          ++ "Part 2: "
          ++ show (part2 txtMap)

type TextMap = [String]

data Velocity = Velocity {vx :: Int, vy :: Int}

data Position = Position {x :: Int, y :: Int}

data Feature = EmptySpace | Tree deriving (Eq, Ord, Show)

part2 :: TextMap -> Int
part2 txtMap =
  let velocitiesToTest = [Velocity {vx = 1, vy = 1}, Velocity {vx = 3, vy = 1}, Velocity {vx = 5, vy = 1}, Velocity {vx = 7, vy = 1}, Velocity {vx = 1, vy = 2}]
      treesFound = map (fromMaybe 1 . Map.lookup Tree . part1 txtMap Position {x = 0, y = 0}) velocitiesToTest
   in product treesFound

part1 :: TextMap -> Position -> Velocity -> Map.Map Feature Int
part1 txtMap position velocity =
  let feature = featureAtMap txtMap position
      nextCounts = part1 txtMap (nextPosition position velocity) velocity
   in if hasReachedEnd txtMap position
        then Map.empty
        else
          Map.fromList
            [ (EmptySpace, (if feature == EmptySpace then 1 else 0) + fromMaybe 0 (Map.lookup EmptySpace nextCounts)),
              (Tree, (if feature == Tree then 1 else 0) + fromMaybe 0 (Map.lookup Tree nextCounts))
            ]

featureAtMap :: TextMap -> Position -> Feature
featureAtMap txtMap Position {x, y} =
  let featureChar = (txtMap !! y) !! x
   in case featureChar of
        '.' -> EmptySpace
        '#' -> Tree
        _ -> error ("Unknown feature on map " ++ show featureChar)

nextPosition :: Position -> Velocity -> Position
nextPosition Position {x, y} Velocity {vx, vy} =
  Position {x = x + vx, y = y + vy}

hasReachedEnd :: TextMap -> Position -> Bool
hasReachedEnd txtMap Position {x, y} =
  length txtMap <= y