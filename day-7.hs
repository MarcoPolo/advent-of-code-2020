{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad (void)
import qualified Data.Bifunctor
import Data.Either (fromRight)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.ParserCombinators.Parsec as Parsec

main =
  do
    testContent <- readFile "test-inputs/day-7"
    testContent2 <- readFile "test-inputs/day-7-2"
    content <- readFile "inputs/day-7"
    putStr "Test Part 1: "
    print (part1 testContent)
    putStr "Part 1: "
    print (part1 content)
    putStr "Test Part 2: "
    print (part2 testContent)
    putStr "Test 2 Part 2: "
    print (part2 testContent2)
    putStr "Part 2: "
    print (part2 content)

newtype Bag = Bag (String, String) deriving (Show, Ord, Eq)

parseRules input =
  let parsed = parse parseRulesGen "bag-rules" input
      graph = Map.fromList $ map (Data.Bifunctor.second Map.fromList) (fromRight [] parsed)
   in graph

parseRulesGen =
  endBy parseRule eol

eol = void (oneOf "\n") <|> eof

parseRule =
  do
    adj <- manyTill anyChar space
    color <- manyTill anyChar space
    string "bags contain "
    rest <- (string "no other bags" >> return []) <|> try (sepBy parseContents (string ", "))
    char '.'
    return (Bag (adj, color), rest)

parseContents =
  do
    many space
    number <- manyTill digit space
    adj <- manyTill alphaNum space
    color <- manyTill alphaNum space
    manyTill anyChar (lookAhead (oneOf ",."))
    return (Bag (adj, color), read number :: Int)

part1 input =
  let graph = parseRules input
   in length $ Set.fromList (findContainingBagFor graph (Bag ("shiny", "gold")))

findContainingBagFor bagGraph bag =
  let containingBagGraph = Map.filter (Map.member bag) bagGraph
      containingBags = Map.keys containingBagGraph
   in containingBags ++ concatMap (findContainingBagFor bagGraph) containingBags

part2 input =
  let graph = parseRules input
   in findBagCountFor graph (Bag ("shiny", "gold"))

findBagCountFor :: Map.Map Bag (Map.Map Bag Int) -> Bag -> Int
findBagCountFor bagGraph bag =
  let bagsIn = bagGraph Map.! bag
      countOfBags = Map.mapWithKey (\k v -> v + (v * findBagCountFor bagGraph k)) bagsIn
   in sum $ Map.elems countOfBags