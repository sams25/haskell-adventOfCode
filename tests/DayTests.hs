module Main where

import Test.HUnit
import qualified System.Exit as Exit
import qualified Day1
import qualified Day2
import qualified Day3

test1a = TestCase (
  assertEqual
  "Day 1a test"
  "142" (
    Day1.solutionA

    "1abc2\n\
    \pqr3stu8vwx\n\
    \a1b2c3d4e5f\n\
    \treb7uchet"
  )
  )

test1b = TestCase (
  assertEqual
  "Day 1b test"
  "281" (
    Day1.solutionB

    "two1nine\n\
    \eightwothree\n\
    \abcone2threexyz\n\
    \xtwone3four\n\
    \4nineeightseven2\n\
    \zoneight234\n\
    \7pqrstsixteen"
  )
  )

test2a = TestCase (
  assertEqual
  "Day 2a test"
  "8" (
    Day2.solutionA

    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
    \Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
    \Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
    \Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
    \Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  )
  )

test2b = TestCase (
  assertEqual
  "Day 2b test"
  "2286" (
    Day2.solutionB

    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
    \Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
    \Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
    \Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
    \Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  )
  )

test3a = TestCase (
  assertEqual
  "Day 3a test"
  "4361" (
    Day3.solutionA
    "467..114..\n\
    \...*......\n\
    \..35..633.\n\
    \......#...\n\
    \617*......\n\
    \.....+.58.\n\
    \..592.....\n\
    \......755.\n\
    \...$.*....\n\
    \.664.598.."
  )
  )

test3b = TestCase (
  assertEqual
  "Day 3b test"
  "467835" (
    Day3.solutionB
    "467..114..\n\
    \...*......\n\
    \..35..633.\n\
    \......#...\n\
    \617*......\n\
    \.....+.58.\n\
    \..592.....\n\
    \......755.\n\
    \...$.*....\n\
    \.664.598.."
  )
  )

allTests :: Test
allTests = TestList [
      TestLabel "test1a" test1a, TestLabel "test1b" test1b
    , TestLabel "test2a" test2a, TestLabel "test2b" test2b
    , TestLabel "test3a" test3a, TestLabel "test3b" test3b
  ]

main :: IO ()
main = do
    Counts _ _ errs fails <- runTestTT allTests
    if errs > 0 || fails > 0 then Exit.exitFailure else Exit.exitSuccess
