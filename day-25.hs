{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.Bifunctor
import Data.Function
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector.Generic as Vec
import qualified Data.Vector.Unboxed as UVec
import Debug.Trace
import Text.ParserCombinators.Parsec as Parsec
import Util

main = do
  putStr "Test Part 1: "
  print $ part1 5764801 17807724
  putStr "Part 1: "
  print $ part1 3469259 13170438

part1 cardPK doorPK =
  handshake doorPK cardSecret
  where
    cardSecret = crackSecret cardPK

handshake subjectNumber loopSize = (subjectNumber ^ loopSize) `mod` 20201227

-- Using Baby step giant sttep https://en.wikipedia.org/wiki/Baby-step_giant-step
discreteLogSolver a b g =
  (foundI * m) + foundJ
  where
    m = sqrt (fromInteger g) & ceiling
    ajTable = Map.fromList $ zip (iterate ((`mod` g) . (* a)) 1) [0 .. m - 1]
    aNegM = modExp a (negate m) g
    gammas = zip [0 .. m - 1] $ iterate ((`mod` g) . (* aNegM)) b
    firstMatch = head $ filter ((`Map.member` ajTable) . snd) gammas
    foundI = fst firstMatch
    foundJ = ajTable Map.! snd firstMatch

modExp :: Integer -> Integer -> Integer -> Integer
-- modExp x y n = mod (x ^ (if y < 0 then mod y (n - 1) else y)) (n)
modExp x y n = mod (x ^ mod y (n - 1)) n

crackSecretNaive publickey = fst $ head $ dropWhile ((/= publickey) . snd) $ zip [1 ..] $ map (handshake 7) [1 ..]

crackSecret publickey = discreteLogSolver 7 publickey 20201227