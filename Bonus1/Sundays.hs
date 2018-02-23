module Sundays where

dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d = mod (d + m' + k + k' + j' + 5 * j ) 7
    where
        k = mod y 100
        j = div y 100
        k' = div k 4
        j' = div j 4

        m' :: Integer
        m' = case m <= 2 of
            True      -> div (13 * (m + 13)) 5
            otherwise -> div (13 * (m + 1)) 5

-- This function recursively checks first day of each month of each year in the given interval
-- and counts sundays.
-- Implemetation without rest expression.
sundays1 :: Integer -> Integer -> Integer
sundays1 start end = sundays' start 1
    where
        sundays' :: Integer -> Integer -> Integer
        sundays' y m
            | y > end   = 0
            | m == 12   = if dayOfWeek y m 1 == 1 then sundays' nextY 1 + 1 else sundays' nextY 1
            | otherwise = if dayOfWeek y m 1 == 1 then sundays' y nextM + 1 else sundays' y nextM
            where
                nextY = y + 1
                nextM = m + 1

-- Implemetation with rest expression.
sundays1Rest :: Integer -> Integer -> Integer
sundays1Rest start end = sundays' start 1
    where
        sundays' :: Integer -> Integer -> Integer
        sundays' y m
            | y > end = 0
            | otherwise = if dayOfWeek y m 1 == 1 then rest + 1 else rest
            where
                nextY = y + 1
                nextM = m + 1
                rest = case m == 12 of
                    True      -> sundays' nextY 1
                    otherwise -> sundays' y nextM
