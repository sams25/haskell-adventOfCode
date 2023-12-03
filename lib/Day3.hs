module Day3
  (
    solutionA,
    solutionB
  )
  where

import Math.Geometry.GridInternal
import Math.Geometry.Grid.Octagonal
import Data.Char (isDigit, digitToInt)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

solution getAnswer input =
  let
    numRows = 1 + count '\n' input
    numCols = fromJust (elemIndex '\n' input)
    grid = rectOctGrid numRows numCols
  in
    getAnswer grid (parseInput input)

solutionA = solution getValidParts
solutionB = solution getGearRatios

-- Parsing the grid
type Part = (Int, [Index RectOctGrid])
type Symbol = (Char, Index RectOctGrid)

parseInput :: String -> ([Part], [Symbol])
parseInput xs = parseInputAcc 1 1 xs [] [] Nothing
  where
    -- End of input
    parseInputAcc _ _ [] parts syms Nothing = (parts, syms)
    parseInputAcc _ _ [] parts syms (Just part) = (part : parts, syms)
    -- Within a part number
    parseInputAcc r c  (x : xs) parts syms (Just part@(num, indices))
      | isDigit x = parseInputAcc r (c + 1) xs parts syms (Just (num * 10 + digitToInt x, (r, c) : indices))
      | x == '.'  = parseInputAcc r (c + 1) xs (part : parts) syms Nothing
      | x == '\n' = parseInputAcc (r + 1) 1 xs (part : parts) syms Nothing
      | otherwise = parseInputAcc r (c + 1) xs (part : parts) ((x, (r, c)) : syms) Nothing
    -- Outside a part number
    parseInputAcc r c (x : xs) parts syms Nothing
      | isDigit x = parseInputAcc r (c + 1) xs parts syms (Just (digitToInt x, [(r, c)]))
      | x == '.'  = parseInputAcc r (c+1) xs parts syms Nothing
      | x == '\n' = parseInputAcc (r + 1) 1 xs parts syms Nothing
      | otherwise = parseInputAcc r (c + 1) xs parts ((x, (r, c)) : syms) Nothing

-- Utility functions
count :: Eq a => a -> [a] -> Int
count thing _ = foldl (\acc elem -> if elem == thing then acc + 1 else acc) 0 _

anyWithin :: Eq a => [a] -> [a] -> Bool
anyWithin [] _ = False
anyWithin (x:xs) check = x `elem` check || anyWithin xs check

-- Problem logic
getValidParts :: RectOctGrid -> ([Part], [Symbol]) -> String
getValidParts grid (parts, syms) =
  let
    symbolLocs = map snd syms
    validParts = [ fst p | p <- parts, anyWithin (neighboursOfSet grid (snd p)) symbolLocs]
  in
    show (sum validParts)

getGearRatios :: RectOctGrid -> ([Part], [Symbol]) -> String
getGearRatios grid (parts, syms) =
  let
    starLocs = [snd s | s <- syms, fst s == '*']
    partNeighbours starLoc =
      [ fst p | p <- parts, starLoc `elem` neighboursOfSet grid (snd p)]
    potentialGearRatios =
      [ partNeighbours (snd s) | s <- syms, fst s == '*' ]
    gearRatios =
      [ product ps | ps <- potentialGearRatios, length ps == 2 ]
  in
    show (sum gearRatios)
