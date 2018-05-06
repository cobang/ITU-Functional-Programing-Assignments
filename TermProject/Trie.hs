module Trie where
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: M.Map Char Trie} deriving (Show)
type Word = String

empty :: Trie
empty = Trie {end=False, children=M.empty}

insert :: Word -> Trie -> Trie
insert [] t        = Trie {end=True, children=children t}
insert (w1':ws') t = Trie {end=(end t), children=(M.insert w1' (insert ws' (fromMaybe empty (children t M.!? w1'))) (children t))}

insertList :: [Word] -> Trie
insertList = undefined

search :: Word -> Trie -> Bool
search = undefined

getWords :: Trie -> [Word]
getWords = undefined

prefix :: Word -> Trie -> Maybe [Word]
prefix = undefined
