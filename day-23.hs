{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

import Control.Concurrent.STM
import Control.Monad
import Data.Either
import Data.Foldable
import Data.Function
import Data.Functor
import Data.IORef as IORef
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.STM.LinkedList as LinkedList
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Set.Ordered as OSet
import qualified Data.Vector as V
import qualified Data.Vector.Generic as Vector
import qualified Data.Vector.Unboxed as UVector
import Debug.Trace
import Text.ParserCombinators.Parsec as Parsec
import Util

main = do
  helperMStr "389125467" "Test Part 1:" part2M
  helperMStr "123487596" "Part 1:" part2M

-- helperM "123487596" "Test Part 1:" part2M

-- helper "inputs/day-22" "Part 1:" part1
-- helper "test-inputs/day-22" "Test Part 2:" part2
-- helper "inputs/day-22" "Part 2:" part2

part1 input = last $ take 100 $ tail $ iterate' applyMove (parsed, 0)
  where
    parse = map (read . (: "")) :: String -> [Int]
    parsed = parse input

part2M input = do
  putStrLn "---"
  -- let foo = setupPuzzlePart [3, 2, 1]
  -- putStrLn $ show foo
  llVec <- initializeLLVec upperBound parsed
  let firstCell = llVec Vector.! (firstVal - 1)
  printLLVec llVec
  putStr "first cell: "
  -- readIORef firstCell >>= return . show >>= putStr
  -- putStrLn ""
  -- let movesApplied = iterate' (applyMove5 llVec) (return firstCell)
  -- let toApply = take (3 + 1) movesApplied
  -- sequence_ toApply
  currentPickedCell <- newIORef firstCell
  -- replicateM_ 2 (applyMove5 llVec (return firstCell))
  replicateM_ rounds (applyMove5 llVec currentPickedCell)
  putStrLn "Applied moves"
  printLLVec llVec
  -- Vector.mapM_ (readIORef >=> return . show >=> putStrLn) $ Vector.take 14 llVec
  print (Vector.length llVec)
  putStrLn "--"
  where
    -- upperBound = 20
    -- rounds = 100
    upperBound = 1000000
    rounds = 10000000
    parsed = parse input ++ [10 .. upperBound]
    parse = map (read . (: "")) :: String -> [Int]
    firstVal = head parsed

printLLVec llVec = do
  let cell1 = llVec Vector.! 0
  -- let nextCells = take (Vector.length llVec) $ iterate' (nextCell llVec) (return cell1)
  let nextCells = take 20 $ iterate' (nextCell llVec) (return cell1)
  vals <- mapM (>>= readIORef >=> return . val) nextCells
  putStr "Vals: "
  print vals

-- applyMove5 :: Vector.Vector v (IORef Cell) => v (IORef Cell) -> IO (IORef Cell) -> IO (IORef Cell)
applyMove5 :: Vector.Vector v (IORef Cell) => v (IORef Cell) -> IORef (IORef Cell) -> IO ()
applyMove5 llVec currentCellRef = do
  -- currentCell <- currentCellIO
  currentCell <- readIORef currentCellRef
  pickedUpCups <- sequence $ take 3 $ drop 1 $ iterate' (nextCell llVec) (return currentCell)
  -- let pickedUpCupsSet = Set.fromList <$> map (readIORef >>= (return . val)) pickedUpCups
  pickedUpCupsSet <- Set.fromList <$> mapM (fmap val . readIORef) pickedUpCups
  currentCellVal <- currentCell & readIORef <&> val
  let nextLowestLabels = tail $ iterate' (nextLowestLabel 1 (Vector.length llVec)) currentCellVal
  let destCupLabel = head $ dropWhile (`Set.member` pickedUpCupsSet) nextLowestLabels
  let lastPickedUp = last pickedUpCups
  -- return currentCell >>= readIORef >>= \v -> traceIO ("xxx Current cup!!!!: " ++ show v)
  -- return lastPickedUp >>= readIORef >>= \v -> traceIO ("xxx Last picked up cup!!!!: " ++ show v)
  lastPickedUpNextIdx <- lastPickedUp & readIORef <&> nextIdx
  -- return lastPickedUp >>= readIORef >>= \v -> traceIO ("xxx Last picked up
  -- cup!!!!: " ++ show v)
  -- printLLVec llVec
  firstPickedUpCupIdx <- currentCell & readIORef <&> nextIdx
  let destCup = llVec Vector.! (((destCupLabel - 1) + Vector.length llVec) `mod` Vector.length llVec)
  -- return destCup >>= readIORef >>= \v -> traceIO ("xxx dest cup!!!!: " ++ show v)
  destCupOriginalNextIdx <- readIORef destCup <&> nextIdx
  currentCell & flip modifyIORef' (\c -> c {nextIdx = lastPickedUpNextIdx})
  lastPickedUp & flip modifyIORef' (\c -> c {nextIdx = destCupOriginalNextIdx})
  destCup & flip modifyIORef' (\c -> c {nextIdx = firstPickedUpCupIdx})

  -- printLLVec llVec

  nextCurrentCell <- nextCell llVec (return currentCell)
  writeIORef currentCellRef nextCurrentCell
  return ()

-- nextCell cell llVec = llVec Vector.! (nextIdx (readIORef cell))
nextCell llVec cell = cell >>= readIORef >>= (return . (llVec Vector.!) . nextIdx)

-- part2M input = do
--   putStrLn "--"
--   k <- atomically $ llfromList (parsed ++ [10 .. upperBound])
--   -- k <- atomically $ llfromList (parsed)
--   m <- atomically $ newTVar (Map.empty :: Map.Map Int (LinkedList.Node Int))
--   firstNode <- atomically $ (LinkedList.next . LinkedList.listHead) k
--   ns <- atomically $ listOfNodes (length parsed) (fromJust firstNode) []
--   vs <- atomically $ newTVar ((V.replicate ((length ns) + 2) (snd (head ns))) Vector.// ns)
--   -- vs <- atomically $ newTVar $ V.fromList (map snd (sortOn fst ns))
--   k0 <- atomically $ (LinkedList.next . LinkedList.listHead) k
--   -- putStrLn $ "" ++ (show $ length (V.empty Vector.// [(3, 0)]))
--   atomically $ modifyTVar m (Map.insert 3 (fromJust k0))
--   -- foo <- fromJust $ fmap (atomically . (\n -> LinkedList.insertAfter 15 n)) k0
--   foo <- atomically $ do
--     currentMap <- readTVar vs
--     LinkedList.insertAfter 1000 (currentMap Vector.! 3)
--   xs <- atomically $ LinkedList.toList k
--   -- putStrLn $ show $ length xs
--   putStrLn "--"
--   where
--     upperBound = 100000 - 1
--     parsed = parse input
--     parse = map (read . (: "")) :: String -> [Int]

addNodetoMap n m =
  modifyTVar m (Map.insert (LinkedList.value n) n)

addNodestoMap 0 n m = return ()
addNodestoMap numberOfNodes n m = do
  modifyTVar m (Map.insert (LinkedList.value n) n)
  mNextNode <- LinkedList.next n
  case mNextNode of
    Just nextNode -> addNodestoMap (numberOfNodes - 1) nextNode m
    Nothing -> return ()

listOfNodes 0 n soFar = return soFar
listOfNodes numberOfNodes n soFar =
  let kv = (LinkedList.value n, n)
   in do
        mNextNode <- LinkedList.next n
        case mNextNode of
          Just nextNode -> listOfNodes (numberOfNodes - 1) nextNode (kv : soFar)
          Nothing -> return soFar

-- (finalState4, _) = (trace (show $ atomically $ llfromList parsed) ([1], 0))

part2 input = (nextTwo, finalState3)
  where
    -- upperNumber = 1000000 -- 1 mill
    -- rounds = 10000000 -- 10 mill
    upperNumber = 1000
    -- rounds = 10000
    rounds = 100
    parse = map (read . (: "")) :: String -> [Int]
    -- parsed = parse input ++ [10 .. upperNumber]
    parsed = parse input
    -- (finalState, _) = last $ take rounds $ tail $ iterate' applyMove (parsed, 0)
    -- (finalState2, _) = last $ take rounds $ tail $ iterate' (applyMove2 (length parsed)) (UVector.fromList parsed, 0)
    (finalState3, f) = last $ take rounds $ tail $ iterate' applyMove3 (Seq.fromList parsed, 0)

    -- indexOf1 = elemIndex 1 finalState & maybeToList & (!! 0)
    -- nextTwo = map (finalState !!) [(indexOf1 + 1) `mod` length finalState, (indexOf1 + 2) `mod` length finalState]
    -- indexOf12 = Vector.elemIndex 1 finalState2 & maybeToList & (!! 0)
    -- nextTwo = map (finalState2 `Vector.unsafeIndex`) [(indexOf12 + 1) `mod` Vector.length finalState2, (indexOf12 + 2) `mod` Vector.length finalState2]
    indexOf1 = Seq.elemIndexL 1 finalState3 & fromJust
    -- indexOf1 = 0
    nextTwo = map (finalState3 `Seq.index`) [(indexOf1 + 1) `mod` Seq.length finalState3, (indexOf1 + 2) `mod` Seq.length finalState3]

applyMove (state, currentCupIdx) =
  (afterMovedPickedCups, (currentCupIdxAfterMove + 1) `mod` length state)
  where
    (preCups, currentCup : restCups) = splitAt currentCupIdx state
    pickedCups = take 3 (restCups ++ preCups)
    allCups = OSet.fromList state
    inPlayCups = (OSet.\\) allCups (OSet.fromList pickedCups)
    nextLowestLabels = tail $ iterate' (nextLowestLabel minLabel maxLabel) currentCup
    destCup = head $ dropWhile (not . (`OSet.member` inPlayCups)) nextLowestLabels
    destCupIdx = OSet.findIndex destCup inPlayCups & maybeToList & (!! 0)
    (preDestCups, _destCup : afterDestCup) = splitAt destCupIdx (toList inPlayCups)
    afterMovedPickedCups = preDestCups ++ destCup : pickedCups ++ afterDestCup
    currentCupIdxAfterMove = elemIndex currentCup afterMovedPickedCups & maybeToList & (!! 0)
    -- currentCupIdxAfterMove = 0
    maxLabel = maximum state
    minLabel = minimum state

-- Linear. And (currently) wrong
applyMove2 :: Vector.Vector v Int => Int -> (v Int, Int) -> (v Int, Int)
applyMove2 stateSize (state, currentCupIdx) =
  (nextState, (currentCupIdx + 1) `mod` Vector.length state)
  where
    -- stateSize = Vector.length state
    pickedCups = map ((state `Vector.unsafeIndex`) . (`mod` stateSize) . (+ currentCupIdx)) [1, 2, 3]
    pickedCupsSet = Set.fromList pickedCups
    currentCup = Vector.unsafeIndex state currentCupIdx
    -- nextLowestLabels = tail $ iterate' (nextLowestLabel minLabel maxLabel) currentCup
    -- destCup = head $ dropWhile (`Set.member` pickedCupsSet) nextLowestLabels
    nextState = Vector.unfoldrN stateSize (\seed -> Just (Vector.unsafeIndex state (seed `mod` stateSize), seed + 1)) 0

    maxLabel = stateSize
    minLabel = 1

applyMove3 :: (Seq.Seq Int, Int) -> (Seq.Seq Int, Int)
applyMove3 (state, currentCupIdx) =
  (nextState, nextIdx)
  where
    stateSize = Seq.length state
    pickedCups = map ((state `Seq.index`) . (`mod` stateSize) . (+ currentCupIdx)) [1, 2, 3]
    -- pickedCupsSet = Set.fromList (trace (show pickedCups ++ " " ++ show nextState0) pickedCups)
    pickedCupsSet = Set.fromList pickedCups
    currentCup = Seq.index state currentCupIdx
    maxLabel = stateSize
    minLabel = 1
    nextLowestLabels = tail $ iterate' (nextLowestLabel minLabel maxLabel) currentCup
    destCup = head $ dropWhile (`Set.member` pickedCupsSet) nextLowestLabels
    -- destCupIdx = Seq.elemIndexL (trace (show (pickedCups, destCup)) destCup) state & maybeToList & (!! 0)

    -- destCupIdx = 0
    -- nextState0 = deleteManyFromSeq (incAndWrap stateSize currentCupIdx) 3 state
    (beforeCurrentCup, afterCurrentCup) = Seq.splitAt (currentCupIdx + 1) state
    nextState0 = Seq.drop (3 - length afterCurrentCup) beforeCurrentCup Seq.>< Seq.drop 3 afterCurrentCup
    destCupIdx1 = Seq.elemIndexL destCup nextState0 & maybeToList & (!! 0)
    (before, after) = Seq.splitAt (destCupIdx1 + 1) nextState0
    nextState = before Seq.>< Seq.fromList pickedCups Seq.>< after
    nextIdx = fmap (incAndWrap $ Seq.length nextState) (Seq.elemIndexL currentCup nextState) & maybeToList & (!! 0)

-- destCupIdx = 0
-- nextState = insertSeqAt (incAndWrap stateSize (trace (show destCup ++ " " ++
-- show destCupIdx1 ++ " " ++ show nextState0) destCupIdx1)) pickedCups
-- nextState0

data Cell = Cell {nextIdx :: Int, val :: Int} deriving (Show, Ord, Eq)

initializeLLVec :: Int -> [Int] -> IO (V.Vector (IORef Cell))
initializeLLVec stateSize initialState = Vector.mapM newIORef $ V.fromListN stateSize $ sortedPuzzlePart ++ restCells
  where
    (puzzlePart, rest) = splitAt 9 initialState
    restCells_ = zipWith (\i v -> (Cell {nextIdx = (i) `mod` stateSize, val = v})) [10 ..] rest
    restCells = if null restCells_ then [] else init restCells_ ++ [(last restCells_) {nextIdx = firstPartOfPuzzleIdx}]
    sortedPuzzlePart_ = setupPuzzlePart puzzlePart
    lastPartOfPuzzle = last puzzlePart
    firstPartOfPuzzle = head puzzlePart
    lastPartOfPuzzleIdx = findIndex (\c -> val c == lastPartOfPuzzle) sortedPuzzlePart_ & fromJust
    firstPartOfPuzzleIdx = findIndex (\c -> val c == firstPartOfPuzzle) sortedPuzzlePart_ & fromJust
    -- _ = (trace (show $last PartOfPuzzle) lastPartOfPuzzle)
    replaceLastPart Cell {nextIdx, val}
      | val == lastPartOfPuzzle = Cell {nextIdx = 9, val = val}
    replaceLastPart c = c

    -- sortedPuzzlePart = init sortedPuzzlePart_ ++ [(((last sortedPuzzlePart_) {nextIdx = if null restCells then nextIdx $ last sortedPuzzlePart else 9}))]
    -- sortedPuzzlePart = if null restCells then sortedPuzzlePart_ else init sortedPuzzlePart_ ++ [(((last sortedPuzzlePart_) {nextIdx = 9}))]
    sortedPuzzlePart = if null restCells then sortedPuzzlePart_ else map replaceLastPart sortedPuzzlePart_

setupPuzzlePart xs = formatted
  where
    sortedxs = sort xs
    -- formatted = map (\x -> elemIndex x xs & fromJust) xs
    nextNumPairs = Map.fromList $ zip xs (tail $ cycle xs)
    numToIdx = Map.fromList $ map (\x -> (x, fromJust $ x `elemIndex` sortedxs)) xs
    formatted = map (\val -> (Cell {val, nextIdx = numToIdx Map.! (nextNumPairs Map.! val)})) sortedxs

llfromList xs = do
  ll <- LinkedList.empty
  zipWithM_ LinkedList.append xs (repeat ll)
  return ll

-- applyMove4 :: (LinkedList.LinkedList Int, LinkedList.Node Int) -> (LinkedList.LinkedList Int, LinkedList.Node Int)
-- applyMove4 (state, currentCup) =
--   (state, currentCup)
--   where
--     foo = (LinkedList.append state 1)

-- stateSize = Seq.length state
-- pickedCups = map ((state `Seq.index`) . (`mod` stateSize) . (+ currentCupIdx)) [1, 2, 3]
-- pickedCupsSet = Set.fromList pickedCups
-- currentCup = Seq.index state currentCupIdx
-- maxLabel = stateSize
-- minLabel = 1
-- nextLowestLabels = tail $ iterate' (nextLowestLabel minLabel maxLabel) currentCup
-- destCup = head $ dropWhile (`Set.member` pickedCupsSet) nextLowestLabels
-- destCupIdx = Seq.elemIndexL (trace (show (pickedCups, destCup)) destCup) state & maybeToList & (!! 0)

-- -- destCupIdx = 0
-- nextState0 = deleteManyFromSeq (incAndWrap stateSize currentCupIdx) 3 state
-- destCupIdx1 = Seq.elemIndexL (trace (show (pickedCups, destCup)) destCup) state & maybeToList & (!! 0)
-- -- destCupIdx = 0
-- nextState = insertSeqAt (incAndWrap stateSize destCupIdx1) pickedCups nextState0

incAndWrap modNum incNum = (incNum + 1) `mod` modNum

insertSeqAt int [] original = original
insertSeqAt int (x : xs) original = insertSeqAt (incAndWrap (Seq.length original) int) xs (Seq.insertAt int x original)

deleteManyFromSeq int amt original = iterate' (Seq.deleteAt int) original & take (amt + 1) & last

-- nextState = (Seq.inse state

-- destCupIdx = Vector.elemIndex destCup state & maybeToList & (!! 0)
-- (preDestCups, _destCup : afterDestCup) = splitAt destCupIdx (toList inPlayCups)
-- afterMovedPickedCups = preDestCups ++ destCup : pickedCups ++ afterDestCup
-- currentCupIdxAfterMove = elemIndex currentCup afterMovedPickedCups & maybeToList & (!! 0)
-- -- currentCupIdxAfterMove = 0
-- maxLabel = maximum state
-- minLabel = minimum state

-- destinationCupLabel = if destinationCupLabel_ < minLabel then maxLabel

nextLowestLabel minLabel maxLabel label
  | label - 1 < minLabel = maxLabel
  | otherwise = label - 1