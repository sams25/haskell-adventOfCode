module NumTheory
  (
    greatestCommonDivisor
  , lowestCommonMultiple
  )
  where

greatestCommonDivisor :: Int -> Int -> Int
greatestCommonDivisor a 0 = a
greatestCommonDivisor a b = greatestCommonDivisor b (a `mod` b)

lowestCommonMultiple :: Int -> Int -> Int
lowestCommonMultiple a b = (a * b) `div` greatestCommonDivisor a b
