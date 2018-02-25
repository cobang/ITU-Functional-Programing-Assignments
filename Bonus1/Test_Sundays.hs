import Test.QuickCheck
import Sundays

-- Results of all functions should be equal.
prop_sundays :: Integer -> Integer -> Bool
prop_sundays start end = (a == b) && (a == c) && (a == d)
    where
        a = Sundays.sundays1Tail start end
        b = Sundays.sundays1Rest start end
        c = Sundays.sundays1 start end
        d = Sundays.sundays2 start end
