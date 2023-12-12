module Day03
  (
    solutionA,
    solutionB
  )
  where

import Data.Char (isDigit, digitToInt)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

solution getAnswer input = show (sum (getAnswer (parseInput input)))

solutionA = solution getValidPartNums
solutionB = solution getGearRatios

-- X Y co-ordinates
type Position = (Int, Int)
-- <beginning position> <ending position>
type Rect = (Position, Position)
-- <number> <rectangle spanned by the digits in the number>
type Part = (Int, Rect)
-- <character> <position of character>
type Symbol = (Char, Position)

-- Is a position within a rectangle?
inRect :: Rect -> Position -> Bool
inRect ((begX, begY), (endX, endY)) (x, y) = begX <= x && x <= endX && begY <= y && y <= endY

-- Is a position around a rectangle?
aroundRect :: Rect -> Position -> Bool
aroundRect rect = inRect (getSurroundingRect rect)

-- Gets the start and end of a rectangle of distance 1 around a given rectangle
getSurroundingRect :: Rect -> Rect
getSurroundingRect ((begX, begY), (endX, endY)) = ((begX - 1, begY - 1), (endX + 1, endY + 1))

-- Parsing the grid
parseInput :: String -> ([Part], [Symbol])
parseInput xs = parseInputAcc 1 1 xs [] [] Nothing
  where
    -- End of input
    parseInputAcc _ _ [] parts syms Nothing = (parts, syms)
    parseInputAcc _ _ [] parts syms (Just part) = (part : parts, syms)
    -- Within a part number
    parseInputAcc r c  (x : xs) parts syms (Just part@(num, (beg, end)))
      | isDigit x = parseInputAcc r (c + 1) xs parts syms (Just (num * 10 + digitToInt x, (beg, (r, c))))
      | x == '.'  = parseInputAcc r (c + 1) xs (part : parts) syms Nothing
      | x == '\n' = parseInputAcc (r + 1) 1 xs (part : parts) syms Nothing
      | otherwise = parseInputAcc r (c + 1) xs (part : parts) ((x, (r, c)) : syms) Nothing
    -- Outside a part number
    parseInputAcc r c (x : xs) parts syms Nothing
      | isDigit x = parseInputAcc r (c + 1) xs parts syms (Just (digitToInt x, ((r, c), (r, c))))
      | x == '.'  = parseInputAcc r (c+1) xs parts syms Nothing
      | x == '\n' = parseInputAcc (r + 1) 1 xs parts syms Nothing
      | otherwise = parseInputAcc r (c + 1) xs parts ((x, (r, c)) : syms) Nothing

-- Problem logic
getValidPartNums :: ([Part], [Symbol]) -> [Int]
getValidPartNums (parts, syms) =
  let
    symbolLocs = map snd syms
    nearSymbol rect = any (aroundRect rect) symbolLocs
  in
    [p | (p, rect) <- parts, nearSymbol rect]

getGearRatios :: ([Part], [Symbol]) -> [Int]
getGearRatios (parts, syms) = [g | g <- map gearRatio starLocs, g /= 0]
  where
    starLocs = [snd s | s <- syms, fst s == '*']
    -- Returns the gear ratio for a (star) location that is a gear, and 0 otherwise
    gearRatio loc =
      let
        partNumsNearby loc (p, rect) acc@(num, ps) = if aroundRect rect loc then (num + 1, p : ps) else acc
        (numParts, partNums) = foldr (partNumsNearby loc) (0, []) parts
      in
        if numParts == 2 then product partNums else 0
