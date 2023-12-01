module Main where

import Day1
import Test.HUnit
import qualified System.Exit as Exit

test1 :: Test
test1 = TestCase (
  assertEqual 
  "Day 1a test passes" 
  "142" (
    Day1.solutionFunc
    "1abc2\n\
    \pqr3stu8vwx\n\
    \a1b2c3d4e5f\n\
    \treb7uchet"
  )
  )
 
tests :: Test
tests = TestList [TestLabel "test1" test1]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
