{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad (void)
import Control.Monad.State as MonadState
import qualified Data.Bifunctor
import Data.Either (fromRight)
import Data.Functor
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.ParserCombinators.Parsec as Parsec

main =
  do
    testContent <- readFile "test-inputs/day-8"
    content <- readFile "inputs/day-8"
    putStr "Test Part 1: "
    print (part1 testContent)
    putStr "Part 1: "
    print (part1 content)
    putStr "Test Part 2: "
    print (part2 testContent)
    putStr "Part 2: "
    print (part2 content)

data Op = Nop | Acc | Jmp deriving (Show)

newtype OpWithArg = OpWithArg (Op, Int) deriving (Show)

part1 input =
  let program = parseProgram input
   in runProgramHelper program

runProgramHelper program =
  let programStates = iterate (runProgramStep program) initialProgramState
      (acc, (didNotLoop, _seenPCs)) = runState (runProgramUntilLoopOrEnd (length program) programStates) (False, Set.empty)
   in (acc, didNotLoop)

part2 input =
  let program = parseProgram input
      programMods = map (modifyProgram program) [0 .. length program - 1]
      mods = map runProgramHelper programMods
   in head $ filter snd mods

modifyProgram :: Program -> Int -> Program
modifyProgram program lineToModify =
  let programLine = program !! lineToModify
      nextLine = case programLine of
        OpWithArg (Jmp, acc) -> OpWithArg (Nop, acc)
        OpWithArg (Nop, acc) -> OpWithArg (Jmp, acc)
        _ -> programLine
      (pre, post) = splitAt lineToModify program
   in pre ++ [nextLine] ++ tail post

-- Parser

parseProgram input = case parse parseProgram_ "program.txt" input of
  Left err -> error (show err)
  Right v -> v

eol = void newline <|> eof

parseProgram_ =
  endBy parseLine eol

parseLine = do
  op <- operator
  space
  optional $ char '+'
  num <- many1 (choice [alphaNum, char '-'])
  return (OpWithArg (op, read num :: Int))

operator =
  string "nop" $> Nop
    <|> string "jmp" $> Jmp
    <|> string "acc" $> Acc

-- Evaluator

type Program = [OpWithArg]

type PC = Int

type Acc = Int

type ProgramState = (PC, Acc)

initialProgramState = (0, 0)

runProgramStep :: Program -> ProgramState -> ProgramState
runProgramStep program (pc, acc) = do
  let opWithArg = program !! pc
   in case opWithArg of
        OpWithArg (Nop, _) -> (pc + 1, acc)
        OpWithArg (Jmp, target) -> (pc + target, acc)
        OpWithArg (Acc, modifier) -> (pc + 1, acc + modifier)

runProgramUntilLoopOrEnd :: Int -> [ProgramState] -> MonadState.State (Bool, Set.Set Int) Int
runProgramUntilLoopOrEnd maxPC ((pc, acc) : states) = do
  (isEnd, seenPCs) <- get
  put (isEnd, Set.insert pc seenPCs)
  if Set.member pc seenPCs
    then return acc
    else
      if pc >= maxPC
        then put (True, seenPCs) >> return acc
        else runProgramUntilLoopOrEnd maxPC states