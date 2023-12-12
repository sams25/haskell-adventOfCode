module PiecewiseLinear
  (
    Interval(..)
  , disjoint
  , overlaps

  , IntervalMap

  , PicewiseLinear
  , idPiecewiseLinear

  , fillGaps
  , into
  )
  where

import Data.List (sortBy)
import Data.Function (on)

-- Interval a b represents [a, b-1)
data Interval = Interval { minIncl :: Int, maxExcl :: Int } deriving (Eq, Show)

instance Ord Interval where
  compare (Interval a b) (Interval c d) = if a == c then compare b d else compare a c

disjoint :: Interval -> Interval -> Bool
(Interval a b) `disjoint` (Interval x y) = b <= x || a >= y
overlaps :: Interval -> Interval -> Bool
overlaps x y = not (disjoint x y)

-- Just so that for all elements in N, comparisons with infinity are strict
negInf = minBound :: Int
posInf = maxBound :: Int
minN = 0
maxN = posInf - 1

-- This represents that [a, b) gets mapped to [x, y) linearly
type IntervalMap = (Interval, Interval)

-- Represents a piecewise linear function
-- It is total, the intervals are ordered, and there are no overlaps between the intervals
type PicewiseLinear = [ IntervalMap ]

idPiecewiseLinear = [(Interval minN maxN, Interval minN maxN)]

-- Takes a piecewise linear map and converts it into a total function on the integers
-- by filling in the gaps with the identity function
fillGaps :: PicewiseLinear -> PicewiseLinear
fillGaps pieces =
  let
    fillGapsStep piece [] = [piece]
    fillGapsStep piece@(Interval a1 b1, _) rest@((Interval a2 b2, _) : _)
      | b1 == a2 = piece : rest
      | b1 < a2  = piece : (Interval b1 a2, Interval b1 a2) : rest
      | b1 > a2  = error ("cannot deal with intervals " ++ show piece ++ " and " ++ show (Interval a2 b2))
    orderedPieces = sortBy (compare `on` fst) pieces
    noGaps = foldr fillGapsStep [] orderedPieces
    ((Interval beg _, _), (Interval _ end, _)) = (head noGaps, last noGaps)
    begFill = if beg > minN then (Interval minN beg, Interval minN beg) : noGaps else noGaps
    endFill = if end < maxN then begFill ++ [(Interval end maxN, Interval end maxN)] else begFill
  in
    condense endFill

-- Glues together consecutive pieces with the same slope
condense :: PicewiseLinear -> PicewiseLinear
condense (first@(Interval a1 b1, Interval x1 y1) : second@(Interval a2 b2, Interval x2 y2) : rest) =
  if b1 == a2 && y1 == x2 then (Interval a1 b2, Interval x1 y2) : rest else
    first : condense (second : rest)
condense other = other

-- Puts f into g and then gets the function f;g
into :: PicewiseLinear -> PicewiseLinear -> PicewiseLinear
f `into` g = condense $ concatMap (remap g) f

remap :: PicewiseLinear -> IntervalMap -> PicewiseLinear
-- Applies the total function g after the interval mapping [a, b) -> [x, y)
-- and creates the resulting piecewise linear function defined on [a, b)
remap g (Interval a b, Interval x y)
  -- full overlap
  | p<=x && q>=y = [(Interval a b,Interval (s+x-p) (s+y-p))]
  -- partial overlap
  | p<=x && q<y = (Interval a (b-y+q),Interval (s+x-p) t):remap rest (Interval (b-y+q) b,Interval q y)
  | otherwise = error "only a partial function was passed to remap"
  where
    overlapping = dropWhile (\im -> Interval x y `disjoint` fst im) g
    -- (a, b) -> (x, y) is composed with (p, q) -> (s, t)
    (Interval p q, Interval s t) = head overlapping
    rest = tail overlapping
