{-# LANGUAGE FlexibleContexts #-}

import Control.Parallel.Strategies
import qualified Data.Bifunctor
import Data.Bits
import Data.Char (digitToInt)
import Data.Either
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Maybe (fromJust, listToMaybe)
import Numeric (readInt)
import qualified Text.Parsec.Number as Number
import Text.ParserCombinators.Parsec as Parsec
import Util

main =
  do
    helper "test-inputs/day-14" "Test Part 1:" part1
    helper "inputs/day-14" "Part 1:" part1
    helper "test-inputs/day-14-2" "Test Part 2:" part2
    helper "inputs/day-14" "Part 2:" part2

part1 input =
  (sum $ Map.elems finalState, maskFn2Helper "XX", leftPad 10 $ toBin 4)
  where
    program = parseInput input
    finalState = foldl (\state programLine -> programLine state) Map.empty program

part2 input =
  sum $ Map.elems finalState
  where
    program = parseInput2 input
    finalState = foldl (\state programLine -> programLine state) Map.empty program

-- part1 = parseInput

replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map $ \c -> if c == a then b else c

readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

parseInput input =
  blockFns
  where
    blocks = tail $ splitOn "mask = " input
    blockFns = concat $ map parseInputBlock blocks

parseInputBlock input =
  programFns
  where
    (mask : prog) = lines input
    programFns = map (parseProgramLine $ maskFn mask) prog

parseProgramLine :: (Int -> Int) -> [Char] -> Map.Map Int Int -> Map.Map Int Int
parseProgramLine maskFn l =
  Map.insert addr (maskFn val)
  where
    (addr, val) = fromRightErr $ parse parser "program line" l :: (Int, Int)
    parser = do
      string "mem["
      address <- manyTill digit (char ']')
      string " = "
      val <- manyTill digit eof
      return (read address :: Int, read val :: Int)

parseInput2 input =
  blockFns
  where
    blocks = tail $ splitOn "mask = " input
    blockFns = concat $ map parseInputBlock2 blocks

parseInputBlock2 input =
  programFns
  where
    (mask : prog) = lines input
    programFns = map (parseProgramLine2 $ maskFn2 mask) prog

parseProgramLine2 :: (Int -> [Int]) -> [Char] -> Map.Map Int Int -> Map.Map Int Int
parseProgramLine2 maskFn l state =
  foldl (\state addr -> Map.insert addr val state) state (maskFn addr)
  where
    (addr, val) = fromRightErr $ parse parser "program line" l :: (Int, Int)
    parser = do
      string "mem["
      address <- manyTill digit (char ']')
      string " = "
      val <- manyTill digit eof
      return (read address :: Int, read val :: Int)

fromRightErr (Left err) = error (show err)
fromRightErr (Right x) = x

maskFn :: String -> Int -> Int
maskFn mask =
  \x -> (x .|. maskOr) .&. maskAnd
  where
    maskOr = fromJust (readBin $ replace 'X' '0' mask)
    maskAnd = fromJust (readBin $ replace 'X' '1' mask)

-- maskFn2 :: String
maskFn2 mask x =
  map (fromJust . readBin) $ maskFn2Helper masked
  where
    xBin = leftPad36 $ toBin x
    masked = zipWith applyMask2 mask xBin

applyMask2 maskChar xChar
  | maskChar == '1' = '1'
  | maskChar == '0' = xChar
  | maskChar == 'X' = 'X'

maskFn2Helper ('X' : []) = ["0", "1"]
maskFn2Helper (h : []) = [(h : [])]
maskFn2Helper (h : mask)
  | h == '0' || h == '1' = map ((:) h) $ maskFn2Helper mask
  | h == 'X' = concat [map ((:) '1') $ maskFn2Helper mask, map ((:) '0') $ maskFn2Helper mask]

toBin 0 = "0"
toBin n
  | n `mod` 2 == 1 = toBin (n `div` 2) ++ "1"
  | n `mod` 2 == 0 = toBin (n `div` 2) ++ "0"

leftPad num s
  | num < length s = s
  | otherwise = take (num - length s) (repeat '0') ++ s

leftPad36 = leftPad 36