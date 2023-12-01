import System.Environment (getArgs)
import qualified Day1

solveProblem solutionFunc inputFile = do
  input <- readFile inputFile
  putStrLn (solutionFunc input)

main = mainWith solutionFunc
  where mainWith solutionFunc = do
          args <- getArgs
          case args of
            [inputFile] -> solveProblem solutionFunc inputFile
            _ -> putStrLn "error: pass an input file"
        solutionFunc = Day1.solutionFunc
