module Day1
  (
    solution1a,
    solution1b
  )
  where

import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf)

solution1 :: (String -> Int) -> (String -> Int) -> String -> String
solution1 firstDigit lastDigit input = 

  show (sum calibrationValues)

  where 
    calibrationValues = map extractCalibrationValue (lines input)

    extractCalibrationValue xs =
      let a = firstDigit xs
          b = lastDigit xs
      in 10 * a + b

solution1a = solution1 firstDigitVanilla (\xs -> firstDigitVanilla (reverse xs))
solution1b = solution1 (firstDigitWithWords id) (\xs -> firstDigitWithWords reverse (reverse xs))

firstDigitVanilla :: String -> Int
firstDigitVanilla (x:xs) = if isDigit x then digitToInt x else firstDigitVanilla xs

firstDigitWithWords :: (String -> String) -> String ->  Int
firstDigitWithWords preOperation all@(x:xs) = 
  if isDigit x 
  then digitToInt x 
  else if (preOperation "one"  ) `isPrefixOf` all then 1
  else if (preOperation "two"  ) `isPrefixOf` all then 2
  else if (preOperation "three") `isPrefixOf` all then 3
  else if (preOperation "four" ) `isPrefixOf` all then 4
  else if (preOperation "five" ) `isPrefixOf` all then 5
  else if (preOperation "six"  ) `isPrefixOf` all then 6
  else if (preOperation "seven") `isPrefixOf` all then 7
  else if (preOperation "eight") `isPrefixOf` all then 8
  else if (preOperation "nine" ) `isPrefixOf` all then 9
  else firstDigitWithWords preOperation xs

