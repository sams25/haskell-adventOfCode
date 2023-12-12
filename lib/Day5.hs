module Day5
  (
    solutionA,
    solutionB
  )
  where

import PiecewiseLinear
import Data.List (foldl', sort, sortBy, find)
import Data.List.Split (endBy)
import Data.Char (isDigit)
import Data.Function (on)

solutionA input =
  let
    (seeds, conversions) = parseSeeds input
    locations = foldl' applyConversion seeds conversions

    applyConversion :: [Int] -> [Triplet] -> [Int]
    applyConversion xs rangeMaps = map toInt thingsOut
      where
        thingsIn = map Pre xs
        thingsOut = foldl' (\ts rangeMap -> map (convert rangeMap) ts) thingsIn rangeMaps
  in
    show $ minimum locations

solutionB input =
  let
    (seedRanges, conversions) = parseSeedRanges input
    picewiseLinears = map createPiecwiseLinear conversions
    seedToLocation = foldl' into idPiecewiseLinear picewiseLinears
    (seed, location) = minimise seedToLocation seedRanges
  in
    show location

type Triplet = (Int, Int, Int)

-------------------------------------------------
-- Part B logic (quicker, can override part A) --
-------------------------------------------------

parseSeedRanges :: String -> ( [Interval], [ [Triplet] ] )
parseSeedRanges input =
  let
    (seeds, conversions) = parseSeeds input
    seedRanges = coupleUp seeds
      where
        coupleUp [] = []
        coupleUp (x:y:xs) = createIntervalFromRange x y : coupleUp xs
  in (seedRanges, conversions)

createIntervalFromRange :: Int -> Int -> Interval
createIntervalFromRange start len = Interval start (start + len)

createPiecwiseLinear :: [Triplet] -> PicewiseLinear
createPiecwiseLinear rangeMaps =
  let
    rangeToInterval (dst, src, len) = (createIntervalFromRange src len, createIntervalFromRange dst len)
    pieces = map rangeToInterval rangeMaps
  in fillGaps pieces

-- For a total function f defined piecewise linearly and some ranges,
-- find an (x, f(x)) such that x is in the ranges and f(x) is minimised
minimise :: PicewiseLinear -> [Interval] -> (Int, Int)
minimise f ranges = minInner (sortBy (compare `on` snd) f) (sort ranges)
  where
    minInner ((from@(Interval a _), Interval x _) : rest) rs =
      case find (from `overlaps`) rs of
        Nothing -> minInner rest rs
        Just (Interval p _) -> if p > a then (p, x + p - a) else (a, x)

--------------------------
-- Part A logic (naive) --
--------------------------

parseSeeds :: String -> ( [Int], [ [Triplet] ] )
parseSeeds input =
  let
    paragraphs = endBy "\n\n" input
    (start:rest) = map (dropWhile (not . isDigit)) paragraphs
    seeds = map read (words start) :: [Int]
    ranges = map getRanges rest
    getRanges chunk = map getTriple (lines chunk)
    getTriple line = listToTriple $ map read (words line) :: (Int, Int, Int)
  in (seeds, ranges)
  where
    listToTriple [a, b, c] = (a, b, c)

-- Has an entity been processed or not?
data Thing = Pre Int | Post Int deriving (Show)

toInt :: Thing -> Int
toInt (Pre x) = x
toInt (Post x) = x

-- Convert from one entity to the next
convert :: Triplet -> Thing -> Thing
convert _ output@(Post _) = output
convert (dst, src, len) input@(Pre x)
  | src <= x && x < src + len = Post (dst + x - src)
  | otherwise = input

temp = solutionB "seeds: 79 14 55 13\n\
    \\n\
    \seed-to-soil map:\n\
    \50 98 2\n\
    \52 50 48\n\
    \\n\
    \soil-to-fertilizer map:\n\
    \0 15 37\n\
    \37 52 2\n\
    \39 0 15\n\
    \\n\
    \fertilizer-to-water map:\n\
    \49 53 8\n\
    \0 11 42\n\
    \42 0 7\n\
    \57 7 4\n\
    \\n\
    \water-to-light map:\n\
    \88 18 7\n\
    \18 25 70\n\
    \\n\
    \light-to-temperature map:\n\
    \45 77 23\n\
    \81 45 19\n\
    \68 64 13\n\
    \\n\
    \temperature-to-humidity map:\n\
    \0 69 1\n\
    \1 0 69\n\
    \\n\
    \humidity-to-location map:\n\
    \60 56 37\n\
    \56 93 4"
