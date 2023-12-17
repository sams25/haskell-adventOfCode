module Day09
  (
    solutionA,
    solutionB
  )
  where

solution predictionFunc input =
  let rows = parseInput input
  in show $ sum $ map predictionFunc rows

solutionA = solution nextInRow
solutionB = solution previousInRow

parseInput :: String -> [[Int]]
parseInput input = map (map read . words) (lines input)

nextInRow :: [Int] -> Int
nextInRow row
  | all (==0) row = 0
  | otherwise = (last row) + nextInRow (diffsBetween row)

previousInRow :: [Int] -> Int
previousInRow row
  | all (==0) row = 0
  | otherwise = (head row) - previousInRow (diffsBetween row)

diffsBetween :: [Int] -> [Int]
diffsBetween [_] = []
diffsBetween (a : b : rest) = (b - a) : diffsBetween (b : rest)
