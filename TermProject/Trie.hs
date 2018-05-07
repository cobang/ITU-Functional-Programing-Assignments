module Trie where
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: M.Map Char Trie} deriving (Eq, Show)
type Word = String

empty :: Trie
empty = Trie {end=False, children=M.empty}

insert :: Word -> Trie -> Trie
insert [] t        = Trie {end=True, children=children t}
insert (w1':ws') t = Trie {end=end t, children=M.insert w1' (insert ws' $ fromMaybe empty $ children t M.!? w1') (children t)}

insertList :: [Word] -> Trie
insertList = undefined

search :: Word -> Trie -> Bool
search = undefined

getWords :: Trie -> [Word]
getWords t = getNode "" [] t
    where
        getNode :: Word -> [Word] -> Trie -> [Word]
        getNode acc l t' = foldr (\x y-> (getEdge acc [] x) ++ y) l (M.toList (children t'))
            where
                getEdge :: Word -> [Word] -> (Char, Trie) -> [Word]
                getEdge acc'' accl'' (c'',t'') = case (end t'', (children t'')==M.empty) of
                    (_, True)       -> (reverse w): accl''
                    (True, False)   -> getNode w ((reverse w):accl'') t''
                    (False, False)  -> getNode w accl'' t''
                    where
                        w = c'':acc''

prefix :: Word -> Trie -> Maybe [Word]
prefix = undefined
