module Anagrams where
import Prelude hiding (Word)
import Data.List
import Data.Char


type Word = [Char]
type Sentence = [Word]
type CharCounts = [(Char, Int)]
