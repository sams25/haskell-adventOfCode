import System.Environment (getArgs)
import Data.Map.Internal ((!))
import AllDays (allSolutions)

solveProblem inputDay =
  let (solutionA, solutionB) = allSolutions ! inputDay
  in do
    -- TODO: Some error handling when the file is not present
    -- (or alternatively, downloading the file from the web)
    input <- readFile ("input/day" ++ show inputDay ++ ".input")
    putStrLn ("Solution A: " ++ solutionA input)
    putStrLn ("Solution B: " ++ solutionB input)

main = do
  args <- getArgs
  case args of
    -- TODO: Some error handling when inputDay is not valid
    [inputDay] -> solveProblem (read inputDay :: Int)
    _ -> putStrLn "error: pass an input day (1-25)"
