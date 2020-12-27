{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Data.Function
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace
import Text.ParserCombinators.Parsec as Parsec
import Util

main =
  do
    helper "test-inputs/day-22" "Test Part 1:" part1
    helper "inputs/day-22" "Part 1:" part1
    helper "test-inputs/day-22" "Test Part 2:" part2
    helper "inputs/day-22" "Part 2:" part2

part1 input = do
  parsed <- parse parser "" input
  return $ part1Helper parsed

part1Helper parsed = calcGameScore finalState
  where
    finalState = last $ takeWhileDifferent $ iterate' playGame parsed

part2 input = do
  parsed <- parse parser "" input
  return $ playGame2 parsed Set.empty

calcScore deck = sum $ zipWith (*) [1 ..] (reverse deck)

calcGameScore ([], deck) = calcScore deck
calcGameScore (deck, []) = calcScore deck
calcGameScore (deck1, deck2)
  | length deck1 > length deck2 = calcScore deck1
  | otherwise = calcScore deck2

type PlayerDeck = [Int]

type GameState = ([Int], [Int])

typeState = ()

playGame (deck1, []) = (deck1, [])
playGame ([], deck2) = ([], deck2)
playGame initialState
  | topCard1 >= topCard2 = (deck1 ++ [topCard1, topCard2], deck2)
  | topCard1 < topCard2 = (deck1, deck2 ++ [topCard2, topCard1])
  where
    (topCard1 : deck1, topCard2 : deck2) = initialState

parser = do
  string "Player 1:\n"
  player1Deck <- parseDeck
  newline
  string "Player 2:\n"
  player2Deck <- parseDeck
  return (player1Deck, player2Deck)

parseDeck = do
  player1Deck <- endBy1 (many1 alphaNum) (void newline <|> eof)
  return (map read player1Deck :: [Int])

-- part2

playGame2 :: GameState -> Set.Set GameState -> (Winner, Int)
playGame2 gameState previousGameRounds
  | Set.member gameState previousGameRounds = (Player1Game, calcScore (topCard1 : deck1))
  | roundWinner `elem` [Player1Game, Player2Game] = (roundWinner, calcGameScore gameState)
  | roundWinner == Player1Round = playGame2 (deck1 ++ [topCard1, topCard2], deck2) nextPrevGameRounds
  | roundWinner == Player2Round = playGame2 (deck1, deck2 ++ [topCard2, topCard1]) nextPrevGameRounds
  where
    --
    -- (topCard1 : deck1, topCard2 : deck2) = (trace ("Game state" ++ (show gameState)) gameState)
    (topCard1 : deck1, topCard2 : deck2) = gameState
    roundWinner = whoWinsRound gameState
    nextPrevGameRounds = Set.insert gameState previousGameRounds

data Winner = Player1Round | Player2Round | Player1Game | Player2Game deriving (Show, Ord, Eq)

whoWinsRound :: GameState -> Winner
whoWinsRound (deck1, []) = Player1Game
whoWinsRound ([], deck2) = Player2Game
whoWinsRound gameState
  | length deck1 >= topCard1 && length deck2 >= topCard2 = gameWinnerAsRound nextGameWinner
  | topCard1 >= topCard2 = Player1Round
  | topCard1 < topCard2 = Player2Round
  where
    (topCard1 : deck1, topCard2 : deck2) = gameState
    (nextGameWinner, _) = playGame2 (take topCard1 deck1, take topCard2 deck2) Set.empty

gameWinnerAsRound Player1Game = Player1Round
gameWinnerAsRound Player2Game = Player2Round