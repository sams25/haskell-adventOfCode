module Day04
  (
    solutionA,
    solutionB
  )
  where

import Data.List.Split (splitOneOf)
import Data.List (intersect, foldl')
import Data.Char (isDigit)

solutionA input = show (sum scores) where scores = map (scoreCard . parseLine) (lines input)
solutionB input = show (scoreAllCards (map parseLine (lines input)))

parseLine :: String -> (Int, [Int], [Int])
parseLine line =
  let
    [prefix, firstSet, secondSet] = splitOneOf ":|" line
    card = read $ dropWhile (not . isDigit) prefix
    winners = map read (words firstSet)
    have = map read (words secondSet)
  in
    (card, winners, have)

numMatchingNumbers winners have = length (winners `intersect` have)

scoreCard (_, winners, have) = if common == 0 then 0 else 2 ^ (common - 1)
  where common = numMatchingNumbers winners have

safeHead [] = 0
safeHead (x:xs) = x

safeTail [] = []
safeTail (x:xs) = xs

addLists [] [] = []
addLists [] ys = ys
addLists xs [] = xs
addLists (x:xs) (y:ys) = (x + y) : addLists xs ys

scoreAllCards :: [(Int, [Int], [Int])] -> Int
scoreAllCards cards = fst $ foldl' step (0, []) cards
  where
    step (count, extras) (_, winners, have) =
      let
        common = numMatchingNumbers winners have
        numCopies = 1 + safeHead extras
      in
      (count + numCopies, safeTail extras `addLists` replicate common numCopies)
