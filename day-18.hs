{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

import Data.Either
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec as Parsec
import Util

main =
  do
    helperStr "2 * 3 + (4 * 5)" "Test Part 1:" part1
    helperStr "2 * 3 + (4 * 5)" "Test Part 2:" part2
    helper "inputs/day-18" "Part 1:" part1
    helper "inputs/day-18" "Part 2:" part2

part1 x = sum results
  where
    results = map (fromRight 0 . evalLine part1Parser . reverseExpr) (lines x)

part2 x = sum results
  where
    results = map (fromRight 0 . evalLine part2Parser) (lines x)

evalLine parser = parse parser "math-homework"

{-
Part 2
factor ::= factor * term
         | term
term ::= atom + term
      | atom
atom ::= '(' factor ')'
      | number
-}

part1Parser =
  try
    ( do
        left <- atom part1Parser
        many space
        op <- oneOf "+*"
        many space
        applyOp op left <$> part1Parser
    )
    <|> atom part1Parser

factor =
  try
    ( do
        left <- term
        many space
        char '*'
        many space
        right <- factor
        return $ left * right
    )
    <|> term

term =
  try
    ( do
        left <- atom factor
        many space
        char '+'
        many space
        right <- term
        return $ left + right
    )
    <|> atom factor

atom topLevel = try (parseParen topLevel) <|> parseNumber

parseParen topLevel = do
  char '('
  expr <- topLevel
  char ')'
  return expr

part2Parser = factor

parseNumber = many1 digit >>= (return . read)

applyOp '+' = (+)
applyOp '*' = (*)

replaceWithMap mapping = map (\c -> Map.findWithDefault c c mapping)

reverseExpr = replaceWithMap (Map.fromList [(')', '('), ('(', ')')]) . reverse