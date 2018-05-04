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

-- charCountsSubsets takes an CharCounts and returns all subsets of given CharCounts
-- first of all I converted given CharCounts to a String because to ease operations
-- for example: fromList [('a',1),('l',2)] -> "all"
-- after that wordSubsets converts a  string to all possible sub strings
-- for example: "all" -> ["all","al","a","ll","l",""]
-- last operations is converting these substrings to CharCounts
charCountsSubsets :: CharCounts -> [CharCounts]
charCountsSubsets cc = map wordCharCounts subWords where
    word = concat (map (\(c, n) -> replicate n c) (toList cc))
    subWords = wordSubsets word where
        wordSubsets :: Word -> [Word]
        wordSubsets []  = [[]]
        wordSubsets (x:xs) = nub (map (x:) (wordSubsets xs) ++ wordSubsets xs)

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

