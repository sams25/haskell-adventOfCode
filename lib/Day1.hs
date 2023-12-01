module Day1
  (
    solutionFunc
  )
  where

import Data.Char

solutionFunc :: String -> String
solutionFunc input = show (sum (map extractCalibrationValue (lines input)))

extractCalibrationValue :: String -> Int
extractCalibrationValue xs = 
  let a = firstDigit xs
      b = firstDigit (reverse xs)
  in 10 * a + b

firstDigit :: String -> Int
firstDigit (x:xs) = if Data.Char.isDigit(x) then Data.Char.digitToInt x else firstDigit xs
