module Anagrams where
import Prelude hiding (Word)
import Data.Char
import Data.List
import Data.Map hiding (map, filter)


type Word = [Char]
type Sentence = [Word]
type CharCounts = Map Char Int

wordCharCounts :: Word -> Map Char Int
wordCharCounts w = fromList l where
    l = zip cs $ map (\c -> length $ filter (== c) lw) cs where
        lw = map toLower w
        cs = nub lw

sentenceCharCounts :: Sentence -> CharCounts
sentenceCharCounts = wordCharCounts.concat

dictCharCounts :: Sentence -> [(Word, CharCounts)]
dictCharCounts d = zip d (map wordCharCounts d)
