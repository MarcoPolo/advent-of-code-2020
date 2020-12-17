{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad
import Data.Ix
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.ParserCombinators.Parsec as Parsec
import Util

main =
  do
    helper "test-inputs/day-16" "Test Part 1:" part1
    helper "inputs/day-16" "Part 1:" part1
    helper "test-inputs/day-16" "Test Part 2:" part2
    helper "inputs/day-16" "Part 2:" part2

part1 input =
  do
    tickets <- parseInput input
    let nearby = nearbyTickets tickets
        fieldRanges = Map.elems (fields tickets)
        validator = \x -> any (any (`inRange` x)) fieldRanges
     in return $ sum $ concatMap (\(Ticket x) -> filter (not . validator) x) nearby

validNearbyTickets tickets =
  let nearby = nearbyTickets tickets
      fieldRanges = Map.elems (fields tickets)
      validator = \x -> any (any (`inRange` x)) fieldRanges
      checkedTickets = map (\(Ticket x) -> Ticket $ filter validator x) nearby
      validTickets = filter (\(Ticket x) -> length x == ticketLength (yourTicket tickets)) checkedTickets
   in validTickets

part2 input =
  do
    tickets_ <- parseInput input
    let validNearby = validNearbyTickets tickets_
        tickets = tickets_ {nearbyTickets = validNearby}
        fieldValidators = Map.map validateField (fields tickets)
        interpret = possibleInterpretations fieldValidators
        interpretations = map interpret (nearbyTickets tickets)
        -- Find the common field names across same tickets
        reducedInterpretationsPt1 = map (foldl1' Set.intersection) $ transpose interpretations
        -- Now whittle down the interpretations
        reducedInterpretationsPt2 = map (head . Set.elems) (reduceInterpretations reducedInterpretationsPt1)
        relevantFields = filter (isDepartureField . fst) (zip reducedInterpretationsPt2 (unwrapTicket (yourTicket tickets)))
     in --
        return $ product $ map snd relevantFields

isDepartureField = isPrefixOf "departure"

ticketLength (Ticket x) = Prelude.length x

validateField fieldDef val = any (`inRange` val) fieldDef

possibleInterpretations _ (Ticket []) = []
possibleInterpretations validators (Ticket (firstField : restField)) =
  Set.fromList possibleInterpretation : possibleInterpretations validators (Ticket restField)
  where
    possibleInterpretation = Map.keys $ Map.filter (\f -> f firstField) validators

-- This looks for a defined field (a field with only one possible
-- interpretation) and uses that to remove possible interpreations from other
-- fields. Then recurses.
reduceInterpretations interpretations
  | interpretations == reduced = reduced
  | otherwise = reduceInterpretations reduced
  where
    definedFields = foldl Set.union Set.empty $ filter ((== 1) . length) interpretations
    reduced = map (reduceFieldByDefinedFields definedFields) interpretations

reduceFieldByDefinedFields definedFields possibleInterpretations
  | length possibleInterpretations == 1 = possibleInterpretations
  | otherwise = Set.difference possibleInterpretations definedFields

newtype Ticket = Ticket [Int] deriving (Show)

data Tickets = Tickets {fields :: Map.Map String [(Int, Int)], yourTicket :: Ticket, nearbyTickets :: [Ticket]} deriving (Show)

unwrapTicket (Ticket ints) = ints

parseInput = (parse parser "tickets-input")

parser =
  do
    fields <- manyTill field newline
    string "your ticket:\n"
    yourTicket <- ticket
    newline
    string "nearby tickets:\n"
    nearbyTickets <- manyTill ticket eof
    return $ Tickets {fields = Map.fromList fields, yourTicket, nearbyTickets}

ticket =
  do
    digits <- sepBy1 (many1 digit) (char ',')
    void newline <|> eof
    return $ Ticket (map read digits :: [Int])

field =
  do
    fieldName <- manyTill anyChar (char ':')
    space
    numRange <- endBy numberRange (try $ void $ string " or " <|> string "\n")
    return (fieldName, numRange)

numberRange =
  do
    low <- many1 digit
    char '-'
    hi <- many1 digit
    return (read low :: Int, read hi :: Int)
