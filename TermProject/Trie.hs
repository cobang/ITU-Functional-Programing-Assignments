module Trie where
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie   = Trie {end :: Bool, children :: M.Map Char Trie} deriving (Eq, Show)
data Action = Add | Search | Find | PrintAll | Exit deriving (Eq, Show)
type Word   = String

empty :: Trie
empty = Trie {end=False, children=M.empty}

insert :: Word -> Trie -> Trie
insert [] t      = Trie {end=True, children=children t}
insert (w1:ws) t = Trie {end=end t, children=M.insert w1 (insert ws $ fromMaybe empty $ children t M.!? w1) (children t)}

insertList :: [Word] -> Trie
insertList = foldr (\x trie -> insert x trie) empty

search :: Word -> Trie -> Bool
search [] t      = end t
search (w1:ws) t = search ws $ fromMaybe empty $ children t M.!? w1

-- getNode is a helper function for getWords and prefix
-- it takes two accumulators, they are word and word list
-- accumulator word is an prefix, it will be added all found words. it can be empty("") or a real prefix("an")
-- accumulator word list is a initial word list to add words in trie
getNode :: Word -> [Word] -> Trie -> [Word]
getNode acc aacl t = foldr (\x y-> (getEdge acc [] x) ++ y) aacl (M.toList (children t))
    where
        getEdge :: Word -> [Word] -> (Char, Trie) -> [Word]
        getEdge acc' accl' (c',t') = case (end t', (children t')==M.empty) of
            (_, True)       -> (reverse w): accl'
            (True, False)   -> getNode w ((reverse w):accl') t'
            (False, False)  -> getNode w accl' t'
            where
                w = c':acc'

-- getWords takes a trie and returns all the words present in the trie
-- it calls helper function getNode with empty("") prefix and root node of trie
getWords :: Trie -> [Word]
getWords t = getNode "" [] t

-- prefix takes a string (prefix) and a trie and it may returns a list of word that start with the given prefix
-- it calls helper function getNode with given prefix and a sub-trie (after traversing or extracting prefix part) if it exist or empty trie
prefix :: Word -> Trie -> Maybe [Word]
prefix w t = if null r then Nothing else Just r where
        r = pre w t where
            pre :: Word -> Trie -> [Word]
            pre [] t'      = getNode (reverse w) [] t'
            pre (w1:ws) t' = pre ws (fromMaybe empty $ children t' M.!? w1)

convertAction :: String -> Action
convertAction = undefined

getInput :: IO String
getInput = do
    line <- getLine
    return line
