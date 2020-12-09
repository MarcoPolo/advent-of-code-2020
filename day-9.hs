main =
  do
    testContent <- readFile "test-inputs/day-9"
    content <- readFile "inputs/day-9"
    putStr "Test Part 1: "
    print $ show (part1 5 testContent)
    putStr "Part 1: "
    print $ show (part1 25 content)
    putStr "Test Part 2: "
    print $ show (part2 5 testContent)
    putStr "Part 2: "
    print $ show (part2 25 content)

parseInput :: String -> [Int]
parseInput = map read . lines

part1 preambleLength input =
  (fst . head) (filter (null . snd) $ reverse (checkNumbers preambleLength numbers))
  where
    numbers = reverse $ parseInput input

checkNumbers preambleLength numbers
  | length preamble /= preambleLength = []
  | otherwise = (target, preamblePairs) : checkNumbers preambleLength (tail numbers)
  where
    preamble = take preambleLength (tail numbers)
    target = head numbers
    preamblePairs = [(x, y) | x <- preamble, y <- preamble, x /= y, x + y == target]

part2 preambleLength input =
  maximum continuousNumbers + minimum continuousNumbers
  where
    numbers = parseInput input
    targetNum = part1 preambleLength input
    sums = map (continousSumEquals targetNum) $ take (length numbers) (iterate tail numbers)
    continuousNumbers = (snd . head) $ filter ((== targetNum) . fst) sums

continousSumEquals targetNum numbers
  | null sums = (0, sums)
  | length sums == 1 = (0, numbersUsed)
  | otherwise = (last sums, numbersUsed)
  where
    sums = takeWhile (<= targetNum) (scanl1 (+) numbers)
    numbersUsed = take (length sums) numbers
