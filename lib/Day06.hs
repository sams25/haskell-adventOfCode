module Day06
  (
    solutionA,
    solutionB
  )
  where

import Data.Char (isDigit)

solutionA input =
  let
    races = parseInput input
    numWays = map numWaysToBeatRace races
  in
    show (product numWays)

solutionB input = solutionA (filter (/= ' ') input)

parseInput :: String -> [(Integer, Integer)]
parseInput input = zip times dists where
  [times, dists] = map (map read . words . dropWhile (not . isDigit)) (lines input)

-- x(T-x) >= D becomes x^2 - Tx +D <= 0
numWaysToBeatRace :: (Integer, Integer) -> Integer
numWaysToBeatRace (time, distance) =
  let
    (l, r) = criticalRangeOfQuadratic 1 (fromIntegral (-time)) (fromIntegral distance)
  in
    greatestIntBelow r - smallestIntAbove l + 1

criticalRangeOfQuadratic a b c =
  let
    disc = sqrt (b^2 - 4*a*c)
    left = ((-b) - disc) / 2
    right = ((-b) + disc) / 2
  in
    (left, right)

isInt x = x == fromInteger (round x)
smallestIntAbove x = if isInt x then round x + 1 else ceiling x
greatestIntBelow x = if isInt x then round x - 1 else floor x
