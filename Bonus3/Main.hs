module Main where
import Prelude hiding (Word, lookup, null)
import Data.Char
import Data.List hiding (lookup, null)
import Data.List.Split
import Data.Map hiding (map, filter)
import System.Environment

type Word = [Char]
type Sentence = [Word]
type CharCounts = Map Char Int

wordCharCounts :: Word -> Map Char Int
wordCharCounts w = fromListWith (+) $ map (\x -> (toLower x, 1)) w

sentenceCharCounts :: Sentence -> CharCounts
sentenceCharCounts = wordCharCounts.concat

dictCharCounts :: Sentence -> [(Word, CharCounts)]
dictCharCounts d = zip d $ map wordCharCounts d

dictWordsByCharCounts :: [(Word, CharCounts)] -> Map CharCounts [Word]
dictWordsByCharCounts x = fromListWith (++) $ map (\(t0, t1) -> (t1, [t0])) x

wordAnagrams :: Word -> Map CharCounts [Word] -> [Word]
wordAnagrams w m = handle (lookup (wordCharCounts w) m) where
    handle :: Maybe [Word] -> [Word]
    handle Nothing  = []
    handle (Just x) = x

charCountsSubsets :: CharCounts -> [CharCounts]
charCountsSubsets cc = map wordCharCounts subWords where
    word     = concat (map (\(c, n) -> replicate n c) (toList cc))
    subWords = nub $ inits word ++ tails word

subtractCounts :: CharCounts -> CharCounts -> CharCounts
subtractCounts cc1 cc2 = fromList $ filter (\(a, b) -> b > 0) $ map (\key -> (key, sub (lookup key cc1) (lookup key cc2))) $ keys cc1 where
    sub :: Maybe Int -> Maybe Int -> Int 
    sub (Just a) (Just b) = a - b
    sub (Just a) Nothing  = a

-- TODO: sentenceAnagrams

main = do
    args <- getArgs
    let sentence = splitOn " " (args !! 0)
    
    content <- readFile ("words.txt")
    let linesOfFiles = lines content
    let dict = dictWordsByCharCounts (dictCharCounts linesOfFiles)

    let anagrams = sentenceAnagrams sentence dict

    putStrLn (show anagrams)

