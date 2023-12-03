module Day2
  (
    solutionA,
    solutionB
  )
  where

import Data.List.Split (endBy)
import Data.Char (isDigit, isLetter)

solutionA input =
  let
    checkRGB = RGB{ r = 12, g = 13, b = 14 }
    parsedLines = map parseLine (lines input)
    possibleGames = [ fst x | x <- parsedLines, possible checkRGB (snd x)]
  in show (sum possibleGames)

solutionB input =
  let
    parsedLines = map parseLine (lines input)
    powers = [ r (snd x) * g (snd x) * b (snd x) | x <- parsedLines]
  in show (sum powers)


data RGB = RGB{ r :: Int, g :: Int, b :: Int } deriving (Show)

defaultRGB :: RGB
defaultRGB = RGB{ r = 0, g = 0, b = 0 }

(+++) :: RGB -> RGB -> RGB
RGB{ r = r1, g = g1, b = b1 } +++ RGB{ r = r2, g = g2, b = b2 } =
  RGB{ r = r1 + r2, g = g1 + g2, b = b1 + b2 }

-- Converts ["1 green", "2 red"] to RGB{ r = 2, g = 1, b = 0 }
getRGB :: [String] -> RGB
getRGB [] = defaultRGB
getRGB (x:xs) =
  let
    num = read (takeWhile isDigit x) :: Int
    colour = head (filter isLetter x)
    thisRGB = case colour of
      'r' -> defaultRGB{ r = num }
      'g' -> defaultRGB{ g = num }
      'b' -> defaultRGB{ b = num }
  in
    thisRGB +++ getRGB xs

-- Finds the maximum of each colour from a list of RGB values
maxRGB :: [RGB] -> RGB
maxRGB xs =
  let
    maxRGBInner acc [] = acc
    maxRGBInner acc (x:xs) = maxRGBInner newAcc xs
      where newAcc = RGB{ r = max (r acc) (r x), g = max (g acc) (g x), b = max (b acc) (b x) }
  in
    maxRGBInner defaultRGB xs

-- Returns a game number, and the maximum number of balls for each colour for that game
parseLine :: String -> (Int, RGB)
parseLine line =
  let
    [start, rest] = endBy ": " line
    gameNumber = read (dropWhile (not . isDigit) start) :: Int
    pulls = endBy "; " rest
    colourInfos = map (getRGB . endBy ", ") pulls
    bestInfo = maxRGB colourInfos
  in (gameNumber, bestInfo)

possible :: RGB -> RGB -> Bool
possible RGB{ r = rMax, g = gMax, b = bMax } RGB{ r = rCheck, g = gCheck, b = bCheck } =
  rCheck <= rMax && gCheck <= gMax && bCheck <= bMax
