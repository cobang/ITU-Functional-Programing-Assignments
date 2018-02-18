module Sundays where

dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d = (d + monthCoefficient m + yearCoefficient y) `mod` 7
    where
        monthCoefficient :: Integer -> Integer
        monthCoefficient m'
            | m' <= 2   = formula (m' + 12)
            | otherwise = formula m'
                where
                    formula :: Integer -> Integer
                    formula m'' = floor (fromIntegral (13 * (m'' + 1)) / 5.0)

        yearCoefficient :: Integer -> Integer
        yearCoefficient y' = yearCo1 y' + yearCo2 y'
            where
                yearCo1 :: Integer -> Integer
                yearCo1 y'' = floor (fromIntegral k / 4.0) + k
                    where
                        k = mod y'' 100

                yearCo2 :: Integer -> Integer
                yearCo2 y'' = yearCo21 j + yearCo22 j
                    where
                        j = floor (fromIntegral y'' / 100.0)

                        yearCo21 :: Integer -> Integer
                        yearCo21 j = floor (fromIntegral j / 4.0)
                
                        yearCo22 :: Integer -> Integer
                        yearCo22 j = j * 5
                        