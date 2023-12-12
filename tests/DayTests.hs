module Main where

import Test.HUnit
import qualified System.Exit as Exit
import Data.Map.Internal ((!))
import AllDays (allSolutions)

-- Template
--,
--(("Expected", -- N, A
--
--  "Input"
-- ),
-- ("Expected", -- N, B
--
--  "Input"
-- )
--)

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
  )
  ,
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
  )
  ,
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
  ,
  (("13", -- 4, A

    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
    \Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
    \Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
    \Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
    \Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
    \Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
   ),
   ("30", -- 4, B

    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
    \Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
    \Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
    \Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
    \Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
    \Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
   )
  )
  ,
  (("35", -- 5, A

    "seeds: 79 14 55 13\n\
    \\n\
    \seed-to-soil map:\n\
    \50 98 2\n\
    \52 50 48\n\
    \\n\
    \soil-to-fertilizer map:\n\
    \0 15 37\n\
    \37 52 2\n\
    \39 0 15\n\
    \\n\
    \fertilizer-to-water map:\n\
    \49 53 8\n\
    \0 11 42\n\
    \42 0 7\n\
    \57 7 4\n\
    \\n\
    \water-to-light map:\n\
    \88 18 7\n\
    \18 25 70\n\
    \\n\
    \light-to-temperature map:\n\
    \45 77 23\n\
    \81 45 19\n\
    \68 64 13\n\
    \\n\
    \temperature-to-humidity map:\n\
    \0 69 1\n\
    \1 0 69\n\
    \\n\
    \humidity-to-location map:\n\
    \60 56 37\n\
    \56 93 4"
   ),
   ("46", -- 5, B

    "seeds: 79 14 55 13\n\
    \\n\
    \seed-to-soil map:\n\
    \50 98 2\n\
    \52 50 48\n\
    \\n\
    \soil-to-fertilizer map:\n\
    \0 15 37\n\
    \37 52 2\n\
    \39 0 15\n\
    \\n\
    \fertilizer-to-water map:\n\
    \49 53 8\n\
    \0 11 42\n\
    \42 0 7\n\
    \57 7 4\n\
    \\n\
    \water-to-light map:\n\
    \88 18 7\n\
    \18 25 70\n\
    \\n\
    \light-to-temperature map:\n\
    \45 77 23\n\
    \81 45 19\n\
    \68 64 13\n\
    \\n\
    \temperature-to-humidity map:\n\
    \0 69 1\n\
    \1 0 69\n\
    \\n\
    \humidity-to-location map:\n\
    \60 56 37\n\
    \56 93 4"
   )
  )
  ,
  (("288", -- 6, A

    "Time:      7  15   30\n\
    \Distance:  9  40  200"
   ),
   ("71503", -- 6, B

    "Time:      7  15   30\n\
    \Distance:  9  40  200"
   )
  )
  ,
  (("6440", -- 7, A

    "32T3K 765\n\
    \T55J5 684\n\
    \KK677 28\n\
    \KTJJT 220\n\
    \QQQJA 483"
   ),
   ("5905", -- 7, B

    "32T3K 765\n\
    \T55J5 684\n\
    \KK677 28\n\
    \KTJJT 220\n\
    \QQQJA 483"
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
