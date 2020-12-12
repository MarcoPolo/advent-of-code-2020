module Util where

helper :: Show x => String -> String -> (String -> x) -> IO ()
helper filepath prefix solver =
  do
    content <- readFile filepath
    putStr prefix
    print $ show (solver content)