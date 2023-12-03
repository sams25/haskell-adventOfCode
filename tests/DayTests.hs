module Main where

import Test.HUnit
import qualified System.Exit as Exit
import Data.Map.Internal ((!))
import AllDays (allSolutions)

-- Template
--
--(("Expected", -- N, A
--
--  "Input"
-- ),
-- ("Expected", -- N, B
--
--  "Input"
-- )
--),

allTestData =
  [
  (("142", -- 1, A

    "1abc2\n\
    \pqr3stu8vwx\n\
    \a1b2c3d4e5f\n\
    \treb7uchet"
   ),
   ("281", -- 1, B

    "two1nine\n\
    \eightwothree\n\
    \abcone2threexyz\n\
    \xtwone3four\n\
    \4nineeightseven2\n\
    \zoneight234\n\
    \7pqrstsixteen"
   )
  ),
  (("8", -- 2, A

    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
    \Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
    \Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
    \Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
    \Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
   ),
   ("2286", -- 2, B

    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
    \Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
    \Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
    \Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
    \Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
   )
  ),
  (("4361", -- 3, A

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
   ),
   ("467835", -- 3, B

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
  ]

createTestForDay :: Int -> ((String, String), (String, String)) -> Test
createTestForDay num ((expA, dataA), (expB, dataB)) =
  let
    label = "Day " ++ show num
    (solutionA, solutionB) = allSolutions ! num
  in
    TestList [
      TestCase (assertEqual (label ++ "A") expA (solutionA dataA)),
      TestCase (assertEqual (label ++ "B") expB (solutionB dataB))
    ]

allTests = TestList (zipWith createTestForDay [1..] allTestData)

main :: IO ()
main = do
    Counts _ _ errs fails <- runTestTT allTests
    if errs > 0 || fails > 0 then Exit.exitFailure else Exit.exitSuccess
