module Day07
  (
    solutionA,
    solutionB
  )
  where

import Data.List (sortBy)
import Data.Function (on)
import Data.Map (fromListWith, toList)
import Data.Char (isDigit)

temp = solutionA "32T3K 765\n\
  \T55J5 684\n\
  \KK677 28\n\
  \KTJJT 220\n\
  \QQQJA 483"

solution withJoker input =
  let
    unsortedHands = map (parseLine withJoker) (lines input)
    sortedHands = sortBy (compare `on` fst) unsortedHands
  in
    show $ sum [snd x * rank | (x, rank) <- zip sortedHands [1..]]

solutionA = solution False
solutionB = solution True

data HandType = HighCard | OnePair | TwoPair | Trips | FullHouse | Quads | Pents deriving (Eq, Ord, Show)

type Card = Int
data Hand = Hand [Card] HandType deriving (Eq, Show)

instance Ord Hand where
  compare (Hand c1 t1) (Hand c2 t2) = if t1 == t2 then compare c1 c2 else compare t1 t2

parseLine :: Bool -> String -> (Hand, Int)
parseLine withJoker line = ((Hand cards (handTypeOf cards)), read bid)
  where
    [cardsText, bid] = words line
    cards = map (parseCard withJoker) cardsText

jokerValue = 1

parseCard :: Bool -> Char -> Card
parseCard withJoker x
  | isDigit x = read [x]
  | x == 'T' = 10
  | x == 'J' = if withJoker then jokerValue else 11
  | x == 'Q' = 12
  | x == 'K' = 13
  | x == 'A' = 14

sortedFrequencies :: (Ord a) => [a] -> [(a, Int)]
sortedFrequencies xs = sortBy (flip compare `on` snd) $ toList $ fromListWith (+) (zip xs (repeat 1))

countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences t xs = length (filter (== t) xs)

handTypeOf :: [Card] -> HandType
handTypeOf cards
  | jokerCount == 5 = Pents
  | otherwise = handTypeFromFrequencies ((topCard, topFrequency + jokerCount) : tail regularFrequencies)
    where
      jokerCount = countOccurrences jokerValue cards
      regularFrequencies = sortedFrequencies (filter (/= jokerValue) cards)
      (topCard, topFrequency) = head regularFrequencies

handTypeFromFrequencies :: [(Int, Int)] -> HandType
handTypeFromFrequencies ((_, 5) : _         ) = Pents
handTypeFromFrequencies ((_, 4) : _         ) = Quads
handTypeFromFrequencies ((_, 3) : (_, 2) : _) = FullHouse
handTypeFromFrequencies ((_, 3) : _         ) = Trips
handTypeFromFrequencies ((_, 2) : (_, 2) : _) = TwoPair
handTypeFromFrequencies ((_, 2) : _         ) = OnePair
handTypeFromFrequencies _ = HighCard
