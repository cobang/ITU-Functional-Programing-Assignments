module Solitarie where
import Data.Char

data Color = Red | Black deriving (Eq, Show)
data Suit  = Clubs | Diamonds | Hearts | Spades deriving (Eq, Show)
data Rank  = Num Int | Jack | Queen | King | Ace deriving (Eq, Show)
data Card  = Card { suit :: Suit, rank :: Rank } deriving (Eq, Show)
data Move  = Draw | Discard Card deriving (Show)
type State = ([Card], [Card], [Move], Int)

cardColor :: Card -> Color
cardColor c = case c of
    Card Spades _ -> Black
    Card Clubs  _ -> Black
    _             -> Red

cardValue :: Card -> Int
cardValue c = case c of
    Card _ (Num n) -> n
    Card _ Ace     -> 11
    _              -> 10

removeCard :: [Card] -> Card -> [Card]
removeCard [] _       = error "c is not in the list"
removeCard (c':cs') c = if c' == c then cs' else c' : removeCard cs' c

allSameColor :: [Card] -> Bool
allSameColor []             = error "there is no card in the list"
allSameColor [c]            = True
allSameColor [c1,c2]        = (cardColor c1) == (cardColor c2)
allSameColor cs@(c1':c2':_) = if (cardColor c1') == (cardColor c2') then allSameColor (tail cs) else False

sumCards :: [Card] -> Int
sumCards cs = sumCards' 0 cs where
    sumCards' :: Int -> [Card] -> Int
    sumCards' acc []       = acc
    sumCards' acc (c1:cs') = sumCards' (acc + cardValue c1) cs'

score :: [Card] -> Int -> Int
score cs g = if allSameColor cs then div preliminary 2 else preliminary where
    sum :: Int
    sum = sumCards cs

    preliminary :: Int
    preliminary = if sum > g then 3*(sum-g) else g-sum

-- Card List cs
-- Move List ms
-- Goal g
-- TODO: Think, Should I add new card to start of the hs' or end'?
runGame :: [Card] -> [Move] -> Int -> Int
runGame cs ms g = step initial where
    initial :: State
    initial = ([], cs, ms, g)

    step :: State -> Int
    step (hs', _, [], g')        = score hs' g'
    step (hs', cs', ms1:ms', g') = case ms1 of
        Discard c -> step ((removeCard hs' c), cs', ms', g')
        Draw      -> if null cs' 
            then score hs' g' 
            else if sumCards (head cs': hs') > g'
                then score hs' g' 
                else step ((head cs': hs'), tail cs', ms', g')

convertSuit :: Char -> Suit
convertSuit c
    | c == 'c' || c == 'C' = Clubs
    | c == 'd' || c == 'D' = Diamonds
    | c == 'h' || c == 'H' = Hearts
    | c == 's' || c == 'S' = Spades
    | otherwise            = error "invalid character for suits"

convertRank :: Char -> Rank
convertRank c
    | c == 'j' || c == 'J' = Jack
    | c == 'q' || c == 'Q' = Queen
    | c == 'k' || c == 'K' = King
    | c == 't' || c == 'T' = Num 10
    | c == '1'             = Ace
    | isDigit c && digitToInt c > 1 && digitToInt c < 10 = Num (digitToInt c)
    | otherwise            = error "invalid character for ranks"

convertCard :: Char -> Char -> Card
convertCard s r = Card (convertSuit s) (convertRank r)

readCards :: IO [Card]
readCards = do line <- getLine
               if line == "."
                  then return []
                  else do rest <- readCards
                          return ([validateAndConvertCard line] ++ rest) where
                            validateAndConvertCard :: String -> Card
                            validateAndConvertCard [c1,c2] = convertCard c1 c2
                            validateAndConvertCard _       = error "invalid string for card"

convertMove :: Char -> Char -> Char -> Move
convertMove m s r
    | m == 'd' || m == 'D' = Draw
    | m == 'r' || m == 'R' = Discard (convertCard s r)

readMoves :: IO [Move]
readMoves = do line <- getLine
               if line == "."
                  then return []
                  else do rest <- readMoves
                          return ([validateAndConvertMove line] ++ rest) where
                            err :: a
                            err = error "invalid string for move"

                            validateAndConvertMove :: String -> Move
                            validateAndConvertMove [c1]       = if (c1 == 'd' || c1 == 'D') then Draw else err
                            validateAndConvertMove [c1,c2,c3] = if (c1 == 'r' || c1 == 'R') then Discard (convertCard c2 c3) else err
                            validateAndConvertMove _          = err

main = do putStrLn "Enter cards:"
          cards <- readCards
          -- putStrLn (show cards)

          putStrLn "Enter moves:"
          moves <- readMoves
          -- putStrLn (show moves)

          putStrLn "Enter goal:"
          line <- getLine

          let goal = read line :: Int

          let score = runGame cards moves goal
          putStrLn ("Score: " ++ show score)
