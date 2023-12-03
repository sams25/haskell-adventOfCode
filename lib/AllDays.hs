module AllDays
  (
    allSolutions
  )
  where

import Data.Map as Map
import qualified Day1
import qualified Day2
import qualified Day3

allSolutions :: Map.Map Int (String -> String, String -> String)
allSolutions = Map.fromList (
  zip [1..] [
      (Day1.solutionA, Day1.solutionB)
    , (Day2.solutionA, Day2.solutionB)
    , (Day3.solutionA, Day3.solutionB)
  ])
