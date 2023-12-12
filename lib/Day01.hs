module Day01
  (
    solutionA,
    solutionB
  )
  where

import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf)

solution :: (String -> Int) -> (String -> Int) -> String -> String
solution firstDigit lastDigit input = show (sum calibrationValues)
  where
    extractCalibrationValue line = 10 * firstDigit line + lastDigit line
    calibrationValues = map extractCalibrationValue (lines input)

solutionA = solution firstDigitVanilla (firstDigitVanilla . reverse)
solutionB = solution (firstDigitWithWords id) (firstDigitWithWords reverse . reverse)

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
