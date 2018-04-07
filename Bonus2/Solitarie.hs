module Solitarie where

data Color = Red | Black deriving (Show)
data Suit  = Clubs | Diamonds | Hearts | Spades deriving (Show)
data Rank  = Num Int | Jack | Queen | King | Ace deriving (Show)
data Card  = Card { suit :: Suit, rank :: Rank } deriving (Show)
data Move  = Draw | Discard Card deriving (Show)

cardColor :: Card -> Color
cardColor c = case c of
    Card Spades _ -> Black
    Card Clubs  _ -> Black
    Card _      _ -> Red 