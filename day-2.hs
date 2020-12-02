import Data.List.Split (splitOn)

main = do
  content <- readFile "inputs/day-2.txt"
  let pws = map parseLine $ lines content
   in putStrLn $
        "Part 1: " ++ (show . length $ filter isValidPw pws) ++ "\n"
          ++ "Part 2: "
          ++ (show . length $ filter isValidPw2 pws)

parseLine :: String -> ((Int, Int), Char, String)
parseLine l = parseWords $ words l

parseWords :: [String] -> ((Int, Int), Char, String)
parseWords (rng : letter : pw : _) = (parseRng rng, head letter, pw)

parseRng :: String -> (Int, Int)
parseRng rng =
  let rangeList = map read (splitOn "-" rng)
   in (head rangeList, head . tail $ rangeList)

countOccurencesIn :: Char -> String -> Int
countOccurencesIn letter = length . filter (letter ==)

isValidPw :: ((Int, Int), Char, String) -> Bool
isValidPw ((low, hi), letter, pw) =
  let occurences = countOccurencesIn letter pw
   in low <= occurences && occurences <= hi

isValidPw2 :: ((Int, Int), Char, String) -> Bool
isValidPw2 ((low, hi), letter, pw) =
  -- All these are equivalent, but I don't understand how
  -- let isMatch = fromMaybe False . fmap (== letter)
  -- let isMatch = (Just letter ==)
  let isMatch = maybe False (== letter)
      lowCharMatch = isMatch $ safeIndex pw (low - 1)
      hiCharMatch = isMatch $ safeIndex pw (hi - 1)
   in lowCharMatch /= hiCharMatch

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | (i > -1) && (length xs > i) = Just (xs !! i)
  | otherwise = Nothing