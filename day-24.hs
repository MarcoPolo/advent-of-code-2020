{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.Bifunctor
import Data.Function
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace
import Text.ParserCombinators.Parsec as Parsec
import Util

main =
  do
    helper "test-inputs/day-24" "Test Part 1:" part1
    helper "inputs/day-24" "Part 1:" part1
    helper "test-inputs/day-24" "Test Part 2:" part2
    helper "inputs/day-24" "Part 2:" part2

part1 x = length finalTiles
  where
    firstLine = head parsed
    parsed = parseInput x
    finalTiles = foldl' flipBackTile Set.empty $ map (bimap (truncateF 4) (truncateF 4) . foldl' moveTurtle initialTurlePos) parsed

flipBackTile blackTileSet pos
  | Set.member pos blackTileSet = Set.delete pos blackTileSet
  | otherwise = Set.insert pos blackTileSet

part2 x = last $ map (Map.size . Map.filter (== Black)) $ take 101 $ iterate' applyStep initialState
  where
    parsed = parseInput x
    flipInstr = map (foldl' moveTurtleTrunc initialTurlePosTrunc) parsed
    finalBlackTiles = foldl' flipBackTile Set.empty flipInstr
    initialState = Map.fromList $ map (,Black) $ Set.toList finalBlackTiles

fillMap :: GameMap -> GameMap
fillMap gameMap =
  foldl' (\acc pos -> Map.insert pos (lookupInMap gameMap pos) acc) Map.empty $ allNeighbors ++ Map.keys gameMap
  where
    allNeighbors = concatMap neighborPosTrunc $ Map.keys $ Map.filter (== Black) gameMap

applyStep :: GameMap -> GameMap
applyStep gameMap =
  Map.mapWithKey (updateTile gameMap) filledGameMap
  where
    filledGameMap = last $ takeWhileDifferent $ iterate' fillMap gameMap

updateTile gameMap tilePos color
  | color == Black && blackNeighborsAmt `elem` (0 : [3 .. 6]) = White
  | color == White && blackNeighborsAmt == 2 = Black
  | otherwise = color
  where
    colorsNearby = neighborColorsTrunc gameMap tilePos
    blackNeighborsAmt = length $ filter (== Black) colorsNearby

neighborPos_ = [E, NE, NW, W, SW, SE]

neighborPosTrunc :: LobbyPos -> [LobbyPos]
neighborPosTrunc tilePos = map (moveTurtleTrunc tilePos) neighborPos_

neighborColorsTrunc gameMap tilePos = map (lookupInMap gameMap . moveTurtleTrunc tilePos) neighborPos_

-- neighbors gameMap tilePos = zip (neighborPos tilePos) (neighborColors gameMap tilePos)

data Color = Black | White deriving (Show, Ord, Eq)

type GameMap = Map.Map LobbyPos Color

lookupInMap gameMap pos = Map.lookup pos gameMap & fromMaybe White

type Pos = (Double, Double)

data LobbyPos = LobbyPos (Double, Double) deriving (Show, Ord, Eq)

fromLobbyPos (LobbyPos pos) = pos

truncatePos :: Pos -> LobbyPos
truncatePos = LobbyPos . bimap (truncateF n) (truncateF n)
  where
    n = 7

truncateF :: Int -> Double -> Double
truncateF places x = fromIntegral (round (x * 10 ^ places)) / 10 ^ places

vertDiff = sin (((2 * pi) / 360) * 60)

vertDiffTrunc = 1

initialTurlePos = (0, 0)

initialTurlePosTrunc = LobbyPos (0, 0)

moveTurtle = moveTurtleWithVertDiff vertDiff

moveTurtleWithVertDiff vertDiff (x, y) E = (x + 1, y)
moveTurtleWithVertDiff vertDiff (x, y) NE = (x + 0.5, y + vertDiff)
moveTurtleWithVertDiff vertDiff (x, y) NW = (x - 0.5, y + vertDiff)
moveTurtleWithVertDiff vertDiff (x, y) W = (x - 1, y)
moveTurtleWithVertDiff vertDiff (x, y) SW = (x - 0.5, y - vertDiff)
moveTurtleWithVertDiff vertDiff (x, y) SE = (x + 0.5, y - vertDiff)

moveTurtleTrunc (LobbyPos pos) dir = LobbyPos $ moveTurtleWithVertDiff vertDiffTrunc pos dir

-- moveTurtle turtlePos dir =

parseInput input = out
  where
    Right out = parse parseInput_ "p" input

parseInput_ = do
  endBy1 parseLine (void newline <|> eof)

data Dir = E | SE | NE | W | SW | NW deriving (Show, Ord, Eq)

parseLine = many1 parseDir

parseDir =
  choice $
    map
      try
      [ string "se" >> return SE,
        string "sw" >> return SW,
        string "ne" >> return NE,
        string "nw" >> return NW,
        string "e" >> return E,
        string "w" >> return W
      ]