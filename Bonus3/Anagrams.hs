module Anagrams where
import Prelude hiding (Word)
import Data.Char
import Data.List
import Data.Map hiding (map, filter)

type Word = [Char]
type Sentence = [Word]
type CharCounts = Map Char Int

wordCharCounts :: Word -> Map Char Int
wordCharCounts w = fromListWith (+) (map (\x -> (x, 1)) w)

sentenceCharCounts :: Sentence -> CharCounts
sentenceCharCounts = wordCharCounts.concat

dictCharCounts :: Sentence -> [(Word, CharCounts)]
dictCharCounts d = zip d (map wordCharCounts d)
