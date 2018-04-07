module Solitarie where

data Color = Red | Black deriving (Show)
data Suit  = Clubs | Diamonds | Hearts | Spades deriving (Eq, Show)
data Rank  = Num Int | Jack | Queen | King | Ace deriving (Eq, Show)
data Card  = Card { suit :: Suit, rank :: Rank } deriving (Eq, Show)
data Move  = Draw | Discard Card deriving (Show)

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
