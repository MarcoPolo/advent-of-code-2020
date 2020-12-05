{-# LANGUAGE NamedFieldPuns #-}

-- import qualified Text.Parsec as Parsec

import Control.Monad (void)
import Data.Either
import Data.List (intercalate, isInfixOf)
import Data.List.Split (splitOn)
import Debug.Trace
import qualified Text.Parsec.Number as Number
import Text.ParserCombinators.Parsec as Parsec

main = do
  testContent <- readFile "test-inputs/day-4"
  testContent2 <- readFile "test-inputs/day-4-2"
  content <- readFile "inputs/day-4"
  putStrLn $ "Test Case 1: " ++ show (part1 testContent)
  putStrLn $ "Part 1: " ++ show (part1 content)
  putStrLn $ "Test Case 2: " ++ show (part2 testContent2)
  putStrLn $ "Part 2: " ++ show (part2 content)

part1 input =
  let passports = map (words . unwords) (splitOn [""] (lines input))
      passportsSanCid = map (filter (not . isInfixOf "cid")) passports
      validPassports = filter ((> 6) . length) passportsSanCid
   in length validPassports

part2 input =
  let passportsList = map unwords (splitOn [""] (lines input))
      passports = map (parse parsePassport "passport-txt") passportsList
      parseablePassports = concat $ snd (partitionEithers passports)
      passportsSanCid = map (filter (\(k, _) -> k /= "cid")) parseablePassports
      validPassports = filter ((> 6) . length) passportsSanCid
   in length validPassports

parsePassport :: Parsec.GenParser Char st [[(String, String)]]
parsePassport = manyTill parsePassportAttrs eof

parsePassportAttrs :: Parsec.GenParser Char st [(String, String)]
parsePassportAttrs =
  do
    attrs <- many1 (choice [parseEcl, parsePid, parseEyr, parseHcl, parseHgt, parseByr, parseIyr, parseCid])
    eol
    return attrs

eol = void (oneOf " \n") <|> eof

parseAttrHelper :: Parsec.GenParser Char st String -> Parsec.GenParser Char st String -> Parsec.GenParser Char st (String, String)
parseAttrHelper key val =
  try $ do
    k <- key
    char ':'
    v <- val
    eol
    return (k, v)

parseEcl =
  parseAttrHelper
    (string "ecl")
    (choice (map (try . string) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]))

parsePid =
  parseAttrHelper
    (string "pid")
    (count 9 digit)

parseEyr =
  parseAttrHelper
    (string "eyr")
    (choice (map (try . string . show) [2020 .. 2030]))

parseIyr =
  parseAttrHelper
    (string "iyr")
    (choice (map (try . string . show) [2010 .. 2020]))

parseByr =
  parseAttrHelper
    (string "byr")
    (choice (map (try . string . show) [1920 .. 2002]))

parseCid =
  parseAttrHelper
    (string "cid")
    (many (noneOf " ") >> return "")

parseHcl =
  parseAttrHelper
    (string "hcl")
    ( do
        char '#'
        count 6 hexDigit
    )

parseHgt =
  parseAttrHelper
    (string "hgt")
    ( try
        ( do
            h <- choice (map (try . string . show) [150 .. 193])
            u <- string "cm"
            return (h ++ u)
        )
        <|> try
          ( do
              h <- choice (map (try . string . show) [59 .. 76])
              u <- string "in"
              return (h ++ u)
          )
    )