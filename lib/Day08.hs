module Day08
  (
    solutionA,
    solutionB
  )
  where

import NumTheory
import qualified Data.Map as M
import Data.List.Split (wordsBy)

solutionA :: String -> String
solutionA input =
  let
    (dirs, graph) = parseInput input
  in
    show (numSteps graph dirs isEndNodeDFA startNodeDFA)

solutionB input =
  let
    (dirs, graph) = parseInput input
    startNodes = startNodesNFA graph
    stepsNeeded = map (numSteps graph dirs isEndNodeNFA) startNodes
  in
    show (foldr lowestCommonMultiple 1 stepsNeeded)

--- Useful types ---

type Graph = M.Map String (String, String)

type DirectionFunc = (String, String) -> String
directionFunc :: Char -> DirectionFunc
directionFunc x = if x == 'L' then fst else snd

--- Setup ---
parseInput :: String -> ([Char], Graph)
parseInput input =
  let
    (dirs : _ : rest) = lines input
    parseNode [a, b, c] = (a, (b, c))
    graph = map (parseNode . wordsBy (`elem` " =(,)")) rest
  in
    (dirs, M.fromList graph)

startNodeDFA :: String
startNodeDFA = "AAA"

startNodesNFA :: Graph -> [String]
startNodesNFA graph = [n | n <- M.keys graph, last n == 'A']

isEndNodeDFA :: String -> Bool
isEndNodeDFA node = node == "ZZZ"

isEndNodeNFA :: String -> Bool
isEndNodeNFA node = last node == 'Z'

-- Traversal ---

numSteps :: Graph -> [Char] -> (String -> Bool) -> String -> Int
numSteps graph allDirs isEndNode startNode =
  let
    follow (d:ds) steps current = follow ds (steps + 1) (directionFunc d $ graph M.! current)
    follow [] steps current = if isEndNode current then steps else follow allDirs steps current
  in
    follow allDirs 0 startNode
