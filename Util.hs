module Util where

helper :: Show x => String -> String -> (String -> x) -> IO ()
helper filepath prefix solver =
  do
    content <- readFile filepath
    putStr prefix
    print $ show (solver content)

helperStr :: Show x => String -> String -> (String -> x) -> IO ()
helperStr content prefix solver =
  do
    putStr prefix
    putStr " "
    putStr content
    putStr " -> "
    print $ show (solver content)