module AllDays
  (
    allSolutions
  )
  where

import Data.Map as Map
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25

allSolutions :: Map.Map Int (String -> String, String -> String)
allSolutions = Map.fromList (
  zip [1..] [
      (Day1.solutionA, Day1.solutionB)
    , (Day2.solutionA, Day2.solutionB)
    , (Day3.solutionA, Day3.solutionB)
    , (Day4.solutionA, Day4.solutionB)
    , (Day5.solutionA, Day5.solutionB)
    , (Day6.solutionA, Day6.solutionB)
    , (Day7.solutionA, Day7.solutionB)
    , (Day8.solutionA, Day8.solutionB)
    , (Day9.solutionA, Day9.solutionB)
    , (Day10.solutionA, Day10.solutionB)
    , (Day11.solutionA, Day11.solutionB)
    , (Day12.solutionA, Day12.solutionB)
    , (Day13.solutionA, Day13.solutionB)
    , (Day14.solutionA, Day14.solutionB)
    , (Day15.solutionA, Day15.solutionB)
    , (Day16.solutionA, Day16.solutionB)
    , (Day17.solutionA, Day17.solutionB)
    , (Day18.solutionA, Day18.solutionB)
    , (Day19.solutionA, Day19.solutionB)
    , (Day20.solutionA, Day20.solutionB)
    , (Day21.solutionA, Day21.solutionB)
    , (Day22.solutionA, Day22.solutionB)
    , (Day23.solutionA, Day23.solutionB)
    , (Day24.solutionA, Day24.solutionB)
    , (Day25.solutionA, Day25.solutionB)
  ])
