import System.Environment (getArgs)
import qualified Day1
import qualified Day2
import qualified Day3

solutionA = Day3.solutionA
solutionB = Day3.solutionB

solveProblem inputFile = do
  input <- readFile inputFile
  putStrLn ("Solution A: " ++ solutionA input)
  putStrLn ("Solution B: " ++ solutionB input)

main = do
  args <- getArgs
  case args of
    [inputFile] -> solveProblem inputFile
    _ -> putStrLn "error: pass an input file"
