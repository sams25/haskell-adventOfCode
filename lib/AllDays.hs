module AllDays
  (
    allSolutions
  )
  where

import Data.Map as Map
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
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
      (Day01.solutionA, Day01.solutionB)
    , (Day02.solutionA, Day02.solutionB)
    , (Day03.solutionA, Day03.solutionB)
    , (Day04.solutionA, Day04.solutionB)
    , (Day05.solutionA, Day05.solutionB)
    , (Day06.solutionA, Day06.solutionB)
    , (Day07.solutionA, Day07.solutionB)
    , (Day08.solutionA, Day08.solutionB)
    , (Day09.solutionA, Day09.solutionB)
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
