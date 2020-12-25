{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.Bifunctor
import Data.Either
import Data.Function
import Data.Functor
import Data.Ix
import Data.List
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace
import Text.ParserCombinators.Parsec as Parsec
import Util

main =
  do
    helper "test-inputs/day-20" "Test Part 1:" part1
    helper "inputs/day-20" "Part 1:" part1
    helperM "test-inputs/day-20" "Test Part 2:" part2
    helperM "inputs/day-20" "Part 2:" part2

part1 input = do
  calcCornerProduct dimension $ head $ fst $ part1Helper input
  where
    parsed = parseImgs input
    dimension = (round . sqrt . fromIntegral) $ Map.size parsed
    upperBound = (dimension - 1, dimension - 1)
    rng = range ((0, 0), upperBound)
    bottomRows = collectColsFor parsed bottomRow
    topRows = collectColsFor parsed topRow
    leftCols = collectColsFor parsed leftCol
    rightCols = collectColsFor parsed rightCol
    borderMatchingFns = map lookupBorderHelper [leftCols, topRows, rightCols, bottomRows]

joinTiles dimension tiles = stackedRows
  where
    rows = chunksOf dimension tiles
    conjoinedRows = map (intercalate "\n" . (map concat . transpose)) rows
    stackedRows = intercalate "\n" conjoinedRows

part1Helper input =
  (solns, dimension)
  where
    solns = findSolnPart1 borderMatchingFns (Map.empty, Set.empty) $ rng
    parsed = parseImgs input
    dimension = (round . sqrt . fromIntegral) $ Map.size parsed
    upperBound = (dimension - 1, dimension - 1)
    rng = range ((0, 0), upperBound)
    bottomRows = collectColsFor parsed bottomRow
    topRows = collectColsFor parsed topRow
    leftCols = collectColsFor parsed leftCol
    rightCols = collectColsFor parsed rightCol
    borderMatchingFns = map lookupBorderHelper [leftCols, topRows, rightCols, bottomRows]

corners dimension = [(0, 0), (0, dimension - 1), (dimension - 1, 0), (dimension - 1, dimension - 1)]

calcCornerProduct dimension puzzleSoFar = product $ map (fst . (puzzleSoFar Map.!)) (corners dimension)

-- [Border] and [BorderMatchingFns] go clockwise from left
findSolnPart1 :: [BorderMatchingFn] -> PuzzleState -> [TilePos] -> [Map.Map TilePos (TileID, Tile)]
findSolnPart1 borderMatchingFns (puzzleSoFar, seenTiles) [] = [puzzleSoFar]
findSolnPart1 borderMatchingFns (puzzleSoFar, seenTiles) (currentPos : restPos)
  | null possibleNextStates = []
  | otherwise = concatMap (\s -> findSolnPart1 borderMatchingFns s restPos) possibleNextStates
  where
    borderForPos = getBorderForTilePos puzzleSoFar currentPos
    possibleTiles = findMatchingTile borderMatchingFns seenTiles borderForPos
    possibleNextStates = map (addTileToState (puzzleSoFar, seenTiles) currentPos) possibleTiles

addTileToState :: PuzzleState -> TilePos -> (TileID, Tile) -> PuzzleState
addTileToState (puzzleSoFar, seenTiles) tilePos (tileID, tile) =
  (Map.insert tilePos (tileID, tile) puzzleSoFar, Set.insert tileID seenTiles)

-- [Border] go clockwise from left
getBorderForTilePos :: Map.Map TilePos (TileID, Tile) -> TilePos -> [Maybe Border]
getBorderForTilePos puzzleSoFar pos =
  zipWith ($) borderFns $ map (fmap snd . (`Map.lookup` puzzleSoFar)) $ neighborPos pos
  where
    borderFns = map fmap [rightCol, bottomRow, leftCol, topRow] -- We want to know what are the borders for this tile. So we get the [l,u,r,d] neighbors and get their matching borders

neighborsDiffs = [(-1, 0), (0, -1), (1, 0), (0, 1)]

neighborPos (x, y) = map (Data.Bifunctor.bimap (x +) (y +)) neighborsDiffs

rotateR img = r
  where
    h = length img
    r = map (reverse . (\i -> map (!! i) img)) [0 .. h - 1]

flipImg = map reverse

parseImgs input =
  lines input
    & splitOn [""]
    & map parseImg
    & Map.fromList

parseImg :: [String] -> (Int, [String])
parseImg (tileNo : imgRows) = (splitOn " " tileNo !! 1 & init & read, imgRows)

imgVariations img = [img | rotation <- take 4 $ iterate rotateR img, img <- [flipImg rotation, rotation]]

type Tile = [String]

type Border = String

type TileID = Int

type TilePos = (Int, Int)

type PuzzleState = (Map.Map TilePos (TileID, Tile), SeenTiles)

type PuzzleState_ = Map.Map (Int, Int) (TileID, Tile)

type TileState = Map.Map TileID Tile

type TileVariations = Map.Map TileID [Tile]

topRow = head

bottomRow = last

leftCol = map head

rightCol = map last

type TileSet = Set.Set TileID

type BorderMatchingFn = Maybe Border -> TileVariations

type SeenTiles = TileSet

findMatchingTile :: [BorderMatchingFn] -> SeenTiles -> [Maybe Border] -> [(TileID, Tile)]
findMatchingTile borderMatchingFns seenTiles borders =
  flattenHelper $
    Map.toList $
      flip Map.difference (Map.fromSet id seenTiles) $
        foldl1' (Map.intersection) $
          map (uncurry ($)) borderFnBorderPairs
  where
    borderFnBorderPairs_ = zip borderMatchingFns borders
    borderFnBorderPairs = if (all isNothing borders) then (borderFnBorderPairs_) else (filter (isJust . snd) borderFnBorderPairs_)

flattenHelper :: [(a, [b])] -> [(a, b)]
flattenHelper = foldl' (\out (tileID, tiles) -> out ++ map (tileID,) tiles) []

printImg = intercalate "\n"

printImgs = unlines . map unwords . transpose

collectColsFor :: TileState -> (Tile -> Border) -> Map.Map Border TileVariations
collectColsFor tileState colFn =
  Map.fromListWith (Map.unionWith (++)) $ concat $ Map.elems $ Map.mapWithKey mapFn tileState
  where
    mapFn = \tileID tile -> map (\t -> (colFn t, Map.singleton tileID [t])) $ imgVariations tile

lookupBorderHelper :: Map.Map Border TileVariations -> BorderMatchingFn
lookupBorderHelper borderMap Nothing = foldl1' (Map.unionWith (++)) $ Map.elems borderMap
lookupBorderHelper borderMap (Just border) = fromMaybe Map.empty $ Map.lookup border borderMap

-- Part2

seaMonsterPattern =
  "                  # \n\
  \#    ##    ##    ###\n\
  \ #  #  #  #  #  #   "

patternToLookupFn :: String -> Map.Map (Int, Int) Char -> (Int, Int) -> Bool
patternToLookupFn patternTemplate imgMap startPos =
  all (\seaMonsterTile -> Map.lookup (shiftByStartPos seaMonsterTile) imgMap == Just '#') seaMonsterTiles
  where
    shiftByStartPos = bimap (fst startPos +) (snd startPos +)
    patternImg = lines patternTemplate
    mapFn = \y x chr -> if chr == '#' then Just (x, y) else Nothing
    seaMonsterTiles = catMaybes $ concat $ zipWith (\y line -> zipWith (mapFn y) [0 ..] line) [0 ..] patternImg :: [(Int, Int)]

inputImgToMap img =
  Map.fromList $
    concat $
      zipWith (\y line -> zipWith (\x chr -> ((x, y), chr)) [0 ..] line) [0 ..] (lines img)

removeBorder img = withoutRight
  where
    withoutTop = tail img
    withoutBottom = init withoutTop
    withoutLeft = map tail withoutBottom
    withoutRight = map init withoutLeft

part2 input = do
  part2Helper firstImg
  putStrLn "--"
  putStrLn $ "rng:" ++ (show rng)
  where
    (firstSoln : _restSolns, dim) = part1Helper input
    firstImg = joinTiles dim $ map (removeBorder . snd . (firstSoln Map.!)) rng
    rng = [(x, y) | y <- [0 .. dim - 1], x <- [0 .. dim - 1]]

part2Helper img = do
  putStr "Amount of sea monsters: "
  print (length $ filter id $ concatMap (\i -> map (lookupFn i) rng) parsedImgs)
  putStr "roughness of water: "
  print (hashesInImg - (hashesInPattern * amountOfSeaMonsters))
  where
    amountOfSeaMonsters = length $ filter id $ concatMap (\i -> map (lookupFn i) rng) parsedImgs
    hashesInImg = length $ filter (== '#') img
    hashesInPattern = length $ filter (== '#') seaMonsterPattern
    imgs = imgVariations (lines img)
    parsedImgs = map (inputImgToMap . unlines) imgs
    lookupFn = patternToLookupFn seaMonsterPattern
    upperBound = (length (head $ lines img) - 1, length (lines img) - 1)
    rng = range ((0, 0), upperBound)