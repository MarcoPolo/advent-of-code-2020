module Util where

helper :: Show x => String -> String -> (String -> x) -> IO ()
helper filepath prefix solver =
  do
    content <- readFile filepath
    putStr prefix
    print $ show (solver content)

helperM filepath prefix solver =
  do
    content <- readFile filepath
    putStrLn prefix
    solver content

helperMStr content prefix solver =
  do
    putStrLn prefix
    putStr " "
    putStr content
    putStr " -> "
    solver content

helperStr :: Show x => String -> String -> (String -> x) -> IO ()
helperStr content prefix solver =
  do
    putStr prefix
    putStr " "
    putStr content
    putStr " -> "
    print $ show (solver content)

takeWhileDifferent xs = head xs : map snd (takeWhile (uncurry (/=)) $ zip xs (tail xs))