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

solution1a = solution1 firstDigitVanilla (firstDigitVanilla . reverse)
solution1b = solution1 (firstDigitWithWords id) (firstDigitWithWords reverse . reverse)

firstDigitVanilla :: String -> Int
firstDigitVanilla (x:xs) = if isDigit x then digitToInt x else firstDigitVanilla xs

firstDigitWithWords :: (String -> String) -> String ->  Int
firstDigitWithWords preOperation all@(x:xs)
  | isDigit x = digitToInt x
  | preOperation "one"   `isPrefixOf` all = 1
  | preOperation "two"   `isPrefixOf` all = 2
  | preOperation "three" `isPrefixOf` all = 3
  | preOperation "four"  `isPrefixOf` all = 4
  | preOperation "five"  `isPrefixOf` all = 5
  | preOperation "six"   `isPrefixOf` all = 6
  | preOperation "seven" `isPrefixOf` all = 7
  | preOperation "eight" `isPrefixOf` all = 8
  | preOperation "nine"  `isPrefixOf` all = 9
  | otherwise = firstDigitWithWords preOperation xs
