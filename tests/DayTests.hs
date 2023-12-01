module Main where

import Day1
import Test.HUnit
import qualified System.Exit as Exit

test1a = TestCase (
  assertEqual 
  "Day 1a test passes" 
  "142" (
    Day1.solution1a

    "1abc2\n\
    \pqr3stu8vwx\n\
    \a1b2c3d4e5f\n\
    \treb7uchet"
  )
  )

test1b = TestCase (
  assertEqual 
  "Day 1b test passes" 
  "281" (
    Day1.solution1b

    "two1nine\n\
    \eightwothree\n\
    \abcone2threexyz\n\
    \xtwone3four\n\
    \4nineeightseven2\n\
    \zoneight234\n\
    \7pqrstsixteen"
  )
  )
 
tests :: Test
tests = TestList [TestLabel "test1a" test1a, TestLabel "test1b" test1b]

main :: IO ()
main = do
    Counts _ _ errs fails <- runTestTT tests
    if errs > 0 || fails > 0 then Exit.exitFailure else Exit.exitSuccess
