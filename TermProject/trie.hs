-- Goksel Coban
-- 150140718

module Main where
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import System.Environment
import System.IO
import Prelude hiding (Word, show)

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
getWords = getNode "" []

-- prefix takes a string (prefix) and a trie and it may returns a list of word that start with the given prefix
-- it calls helper function getNode with given prefix and a sub-trie (after traversing or extracting prefix part) if it exist or empty trie
prefix :: Word -> Trie -> Maybe [Word]
prefix w t = if null r then Nothing else Just r where
        r = pre w t where
            pre :: Word -> Trie -> [Word]
            pre [] t'      = getNode (reverse w) [] t'
            pre (w1:ws) t' = pre ws (fromMaybe empty $ children t' M.!? w1)

convertAction :: Char -> Action
convertAction c
    | elem c "aA" = Add
    | elem c "sS" = Search
    | elem c "fF" = Find
    | elem c "pP" = PrintAll
    | elem c "eE" = Exit
    | otherwise   = error "Unknown action"

listActions :: IO ()
listActions = do
    putStrLn "a) Add Word"
    putStrLn "s) Search Word"
    putStrLn "f) Find word with prefix"
    putStrLn "p) Print all words"
    putStrLn "e) Exit"

getInput :: IO String
getInput = do
    putStrLn "Enter word/prefix"
    line <- getLine
    return line

getAction :: IO Action
getAction = do
    putStrLn "Enter the Action"
    input <- getLine
    if length input == 1 && elem (head input) "aAsSfFpPeE"
        then return $ convertAction $ head input
        else do 
            putStrLn "Please enter a valid character (a,s,f,p,e)"
            getAction

doAction :: Action -> Word-> Trie -> IO Trie
doAction a w t = case a of
    Add      -> do
        let newTrie = insert w t
        putStrLn ("Word " ++ w ++ " added to the Trie")
        return newTrie

    Search   -> do
        if search w t
            then do
                putStrLn "Exists in dictionary!"
            else do
                putStrLn "Not exists!"
        return t

    Find     -> do
        let fw = fromMaybe [] $ prefix w t
        if length fw == 0
            then putStrLn ("NO words found with that prefix!")
            else do
                putStrLn ("Found words:")
                show $ fw
        return t

    PrintAll -> do
        putStrLn ("List of words in dictionary:")
        show $ getWords t
        return t

    otherwise -> error "Inappropriate action"

show :: [Word] -> IO ()
show []      = do return ()
show (w1:ws) = do
    putStrLn w1
    show ws

routine :: Trie -> IO ()
routine t = do
    listActions
    action <- getAction
    if action == Exit
        then
            return ()
        else do
            if action /= PrintAll
                then do
                    input <- getInput
                    trie <- doAction action input t
                    putStrLn ""
                    routine trie
                else do
                    trie <- doAction action "" t
                    putStrLn ""
                    routine trie

main = do
    -- get command line argument
    args <- getArgs
    let fileName = args !! 0

    -- read file and convert to word list
    content <- readFile (fileName)
    let words = lines content

    -- create trie
    let trie = insertList words

    routine trie
