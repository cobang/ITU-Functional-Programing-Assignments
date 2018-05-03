module Anagrams where
import Prelude hiding (Word)
import Data.List
import Data.Char


type Word = [Char]
type Sentence = [Word]
type CharCounts = [(Char, Int)]

wordCharCounts :: Word -> CharCounts
wordCharCounts w = zip cs $ map (\c -> length $ filter (== c) lw) cs where
    lw = map toLower w
    cs = nub lw

sentenceCharCounts :: Sentence -> CharCounts
sentenceCharCounts = wordCharCounts.concat

dictCharCounts :: [Word] -> [(Word, CharCounts)]
dictCharCounts d = zip d (map wordCharCounts d)
