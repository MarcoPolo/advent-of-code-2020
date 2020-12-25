{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Data.Function
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.ParserCombinators.Parsec as Parsec
import Util

main =
  do
    helper "test-inputs/day-21" "Test Part 1:" part1
    helper "inputs/day-21" "Part 1:" part1
    helper "test-inputs/day-21" "Test Part 2:" part2
    helper "inputs/day-21" "Part 2:" part2

transformToAllergenMap (allergens, ingredients) = Map.fromList [(allergen, Set.fromList ingredients) | allergen <- allergens]

part1 input = do
  parsed <- parse parser "" input
  let allergenMap = map transformToAllergenMap parsed & foldl1' (Map.unionWith Set.intersection) & reduceInterpretations
      allAllergenIngredients = allegenIngredientSet allergenMap
      allIngredients = concatMap snd parsed
   in return $ length $ filter (not . (`Set.member` allAllergenIngredients)) allIngredients

part2 input = do
  parsed <- parse parser "" input
  let allergenMap = map transformToAllergenMap parsed & foldl1' (Map.unionWith Set.intersection) & reduceInterpretations
   in return $ sortedAllergens allergenMap

parser =
  endBy1 parseLine (void newline <|> eof)

parseLine = do
  ingredients <- endBy1 parseWord (space <|> char '(')
  string "(contains "
  allergens <- sepBy1 parseWord (string ", ")
  char ')'
  return (allergens, ingredients)

parseWord = many1 alphaNum

reduceInterpretations interpretations
  | interpretations == reduced = reduced
  | otherwise = reduceInterpretations reduced
  where
    definedAllergens = foldl1' Set.union $ Map.elems $ Map.filter ((== 1) . length) interpretations
    mapFn = \x -> if length x == 1 then x else x `Set.difference` definedAllergens
    reduced = Map.map mapFn interpretations

allegenIngredientSet allergenMap = foldl1' Set.union $ Map.elems allergenMap

sortedAllergens allergenMap = intercalate "," sorted
  where
    sorted = concatMap (Set.toList . snd) $ sortOn fst $ Map.toList allergenMap
