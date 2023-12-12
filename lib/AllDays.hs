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

allSolutions :: Map.Map Int (String -> String, String -> String)
allSolutions = Map.fromList (
  zip [1..] [
      (Day1.solutionA, Day1.solutionB)
    , (Day2.solutionA, Day2.solutionB)
    , (Day3.solutionA, Day3.solutionB)
    , (Day4.solutionA, Day4.solutionB)
    , (Day5.solutionA, Day5.solutionB)
    , (Day6.solutionA, Day6.solutionB)
  ])
