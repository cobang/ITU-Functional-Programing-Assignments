module Sundays where

dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d = mod (d + m' + k + k' + j' + 5 * j ) 7
    where
        y' = if m <= 2 then y - 1 else y
        k = mod y' 100
        j = div y' 100
        k' = div k 4
        j' = div j 4
        m' = if m <= 2 then div (13 * (m + 13)) 5 else div (13 * (m + 1)) 5

-- sundays1 functions recursively checks first day of each month of each year in the given interval
-- and counts sundays.
-- Tail recursive implementation.
sundays1Tail :: Integer -> Integer -> Integer
sundays1Tail start end = sundays' 0 start 1
    where
        sundays' :: Integer -> Integer -> Integer -> Integer
        sundays' acc y m
            | y > end   = acc
            | m == 12   = sundays' acc' nextY 1
            | otherwise = sundays' acc' y nextM
            where
                nextY = y + 1
                nextM = m + 1
                acc'  = if dayOfWeek y m 1 == 1 then acc + 1 else acc

-- Implemetation without rest expression.
sundays1 :: Integer -> Integer -> Integer
sundays1 start end = sundays' start 1
    where
        sundays' :: Integer -> Integer -> Integer
        sundays' y m
            | y > end   = 0
            | m == 12   = sundays' nextY 1 + n
            | otherwise = sundays' y nextM + n
            where
                nextY = y + 1
                nextM = m + 1
                n     = if dayOfWeek y m 1 == 1 then 1 else 0

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
                rest = if m == 12 then sundays' nextY 1 else sundays' y nextM

leap :: Integer -> Bool
leap y = (mod y 4 == 0) && (mod y 100 /= 0) || (mod y 400 == 0)

daysInMonth :: Integer -> Integer -> Integer
daysInMonth y m
    | m==2                                   = if leap y then 29 else 28
    | (m==4) || (m==6) || (m==9) || (m== 11) = 30
    | otherwise                              = 31

sundays2 :: Integer -> Integer -> Integer
sundays2 start end = sundays' 0 start 1 dow
    where
        dow = (dayOfWeek start 1 1 - 1) `mod` 7

        sundays' :: Integer -> Integer -> Integer -> Integer -> Integer
        sundays' acc y m weekday
            | y > end   = acc
            | m == 12   = sundays' acc' nextY 1 days
            | otherwise = sundays' acc' y nextM days
            where
                days  = mod (weekday + daysInMonth y m) 7
                nextY = y + 1
                nextM = m + 1
                acc'  = if mod weekday 7 == 0 then acc + 1 else acc

-- Are all days equally possible?
isDaysEqual :: Bool
isDaysEqual = mod (days' 0 1) 7 == 0
    where
        days' :: Integer -> Integer -> Integer
        days' acc y = if y > 400 then acc else days' (mod (acc + daysInYear y) 7) (y + 1)
            where
                daysInYear :: Integer -> Integer
                daysInYear y' = if leap y then 366 else 365
