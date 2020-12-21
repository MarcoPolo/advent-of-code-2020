{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

import Data.Either
import Data.Functor
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace
import Text.ParserCombinators.Parsec as Parsec
import Util

main =
  do
    -- helper "test-inputs/day-19" "Test Part 1:" part1
    helper "inputs/day-19" "Part 1:" part1
    -- helper "test-inputs/day-19-2" "Test Part 2:" part2
    helper "inputs/day-19" "Part 2:" part2

part1 input = length $ filter (isRight . fst) $ zip (map (parse parser "Validator") msgs) msgs
  where
    (parsedRules, msgs) = parseInput input
    parser = ruleToParser parsedRules (parsedRules Map.! 0) eof -- Assert that it's the end of the input

part2 input = length $ filter (isRight . fst) $ zip (map (parse parser "Validator") msgs) msgs
  where
    (parsedRules_, msgs) = parseInput input
    rulePatches = Map.fromList $ rights $ map (parse parseRule "rule-patch") ["8: 42 | 42 8  ", "11: 42 31 | 42 11 31 "]
    parsedRules = Map.union rulePatches parsedRules_
    parser = ruleToParser parsedRules (parsedRules Map.! 0) eof -- Assert that it's the end of the input

ruleToParser :: Map.Map Int Rule -> Rule -> Parser () -> Parser ()
ruleToParser _ (Terminal s) next = void $ string s >> next
ruleToParser rules (Comb ruleRefs) next = choice $ map (try . foldr (ruleToParser rules . (rules Map.!)) next) ruleRefs

data Rule = Terminal String | Comb [[Int]] deriving (Show, Ord, Eq)

parseInput x = case parse parser "day-19" x of
  Right output -> output
  Left err -> error $ show err

parser = do
  rules <- endBy1 parseRule (newline <|> try (newline >> newline))
  spaces
  msgs <- endBy1 (many1 (noneOf "\n")) (void newline <|> eof)
  return (Map.fromList rules, msgs)

parseRule = do
  ruleNo <- many1 digit
  char ':'
  rule <- try parseTerminal <|> try parseComb
  return (read ruleNo :: Int, rule)

parseTerminal = do
  spaces
  char '"'
  termStr <- many1 alphaNum
  char '"'
  return $ Terminal termStr

parseComb = do
  spaces
  ruleRefs <- sepBy1 (endBy1 (many1 digit <&> read) (char ' ' <|> lookAhead newline)) (string "| ")
  return $ Comb ruleRefs
