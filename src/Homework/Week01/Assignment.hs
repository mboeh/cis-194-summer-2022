module Homework.Week01.Assignment where

import Control.Monad (mfilter)
import Data.List (unfoldr)
import Data.Text.Internal.Builder.Int.Digits (digits)
import Data.Tuple (swap)
import Test.QuickCheck (NonEmptyList (NonEmpty))

-- #1a
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev = unfoldr digits
  where
    digits n = splitN <$> mfilter (> 0) (Just n)
    splitN n = swap $ divMod n 10

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . double . reverse
  where
    double (a : b : r) = a : (b * 2) : double r
    double x = x

-- #3
sumDigits :: [Integer] -> Integer
sumDigits x = sum $ map (sum . toDigits) x

-- #4
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther $ toDigits n) `mod` 10 == 0

-- #5
type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
