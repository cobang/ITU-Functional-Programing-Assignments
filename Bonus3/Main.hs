module Main where
import Prelude hiding (Word, lookup)
import Data.Char
import Data.List hiding (lookup, null)
import Data.List.Split
import Data.Map hiding (map, filter, null)
import System.Environment

type Word = [Char]
type Sentence = [Word]
type CharCounts = Map Char Int

-- wordCharCounts takes a Word and return CharCounts
-- CharCounts is a Map Char to Int
-- it represents char occurrences count
-- Ex: "All" -> fromList [('a',1),('l',2)]
-- 
-- it converts given word to tuple list 
-- "All" -> [('a',1),('l',1),('l',1)]
-- after that it sums char occurrences
wordCharCounts :: Word -> CharCounts
wordCharCounts w = fromListWith (+) $ map (\x -> (toLower x, 1)) w

-- sentenceCharCounts does same operation with wordCharCounts for sentences
-- Ex: ["yes", "all"] -> fromList [('a',1),('e',1),('l',2),('s',1),('y',1)]
sentenceCharCounts :: Sentence -> CharCounts
sentenceCharCounts = wordCharCounts.concat

-- firstly dictCharCounts calculates CharCounts of all word
-- secondly it zips words with their CharCounts
-- Ex: ["yes", "all"] -> [("yes",fromList [('e',1),('s',1),('y',1)]),("all",fromList [('a',1),('l',2)])]
dictCharCounts :: Sentence -> [(Word, CharCounts)]
dictCharCounts d = zip d $ map wordCharCounts d

-- dictWordsByCharCounts is list of Map CharCounts to [Word]
-- it gives all words in dictionary which is same with CharCounts
-- to construct these dictionary
-- firstly orders of input tuples are changed (Word, CharCounts) -> (CharCounts, Word) to ease map over same CharCounts
-- ease map over same CharCounts with using fromListWith
-- Ex:
-- input: [("yes",fromList [('e',1),('s',1),('y',1)]),("all",fromList [('a',1),('l',2)])]
-- output: fromList [(fromList [('a',1),('l',2)],["all"]),(fromList [('e',1),('s',1),('y',1)],["yes"])]
dictWordsByCharCounts :: [(Word, CharCounts)] -> Map CharCounts [Word]
dictWordsByCharCounts x = fromListWith (++) $ map (\(t0, t1) -> (t1, [t0])) x

-- toWord converts the given CharCounts to a String
-- for example: fromList [('a',1),('l',2)] -> "all"
toWord :: CharCounts -> Word
toWord cc = concat (map (\(c, n) -> replicate n c) (toList cc))

-- wordAnagrams takes a word and a dictionary, returns all words in the dictionary which have same CharCounts
-- it use lookup function of Map
-- if there is no anagram it returns empty list
wordAnagrams :: Word -> Map CharCounts [Word] -> [Word]
wordAnagrams w m = handle (lookup (wordCharCounts w) m) where
    handle :: Maybe [Word] -> [Word]
    handle Nothing  = []
    handle (Just x) = x

-- charCountsSubsets takes an CharCounts and returns all subsets of given CharCounts
-- first of all I converted given CharCounts to a String because to ease operations
-- after that wordSubsets converts a  string to all possible sub strings
-- for example: "all" -> ["all","al","a","ll","l",""]
-- last operations is converting these substrings to CharCounts
charCountsSubsets :: CharCounts -> [CharCounts]
charCountsSubsets cc = map wordCharCounts subWords where
    subWords = wordSubsets $ toWord cc where
        wordSubsets :: Word -> [Word]
        wordSubsets []  = [[]]
        wordSubsets (x:xs) = nub (map (x:) (wordSubsets xs) ++ wordSubsets xs)

-- subtractCounts takes two CharCounts and subtract second one from first one
-- cc2 is a subset of cc1
-- firstly, we get all keys of cc1
-- secondly, we subtract occurrences count of each keys with using helper sub function
-- sub functions operates subtract operations if both CharCounts has occurrences count to corresponding char
-- or it returns occurrences count of first one
-- lastly, we filter chars which have occurrences count larger than 0
subtractCounts :: CharCounts -> CharCounts -> CharCounts
subtractCounts cc1 cc2 = fromList $ filter (\(a, b) -> b > 0) $ map (\key -> (key, sub (lookup key cc1) (lookup key cc2))) $ keys cc1 where
    sub :: Maybe Int -> Maybe Int -> Int 
    sub (Just a) (Just b) = a - b
    sub (Just a) Nothing  = a

-- sentenceAnagrams takes a Sentence and a dictionary, returns anagrams of the sentence
-- firstly it calculates CharCounts of given sentence
-- after that it calls helper iter function recursively
-- in list comprehension part:
-- -- firstly subsets(sub words) of calculated CharCounts are calculated
-- -- secondly all anagrams of a word are calculated
-- -- after selecting a word, remaining CharCounts are calculated and iter functions called for new words
-- -- at last all selected words constructs a sentence
sentenceAnagrams :: Sentence -> Map CharCounts [Word] -> [Sentence]
sentenceAnagrams s dict = iter (sentenceCharCounts s) where
    iter cc = case size cc of
        0 -> [[]]
        otherwise -> [word: sentence | sub  <- (charCountsSubsets cc),
                                       word <- wordAnagrams (toWord sub) dict,
                                       sentence <- iter (subtractCounts cc sub)]

main = do
    -- get command line argument
    args <- getArgs
    let sentence = splitOn " " (args !! 0)
    
    -- read file and convert to word list
    content <- readFile ("words.txt")
    let words = lines content

    -- create dictionary
    let dict = dictWordsByCharCounts $ dictCharCounts words

    -- get anagrams
    let anagrams = sentenceAnagrams sentence dict
    putStrLn (show anagrams)

