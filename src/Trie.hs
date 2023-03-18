module Trie (put, containsPrefix, contains, Trie (..)) where

import Data.List
import Data.Maybe

data Trie = Root [Trie] | Node [Trie] Char Bool deriving (Show)

put :: [Char] -> Trie -> Trie
put word@(x : xs) (Root children) = case findChild x children of
  Just child -> Root (replaceChild (put xs child) children)
  Nothing -> Root (createSubtree word : children)
put [] (Node children key _) = Node children key True
put (x : xs) (Node children key isComplete) = case findChild x children of
  Just child -> Node (replaceChild (put xs child) children) key isComplete
  Nothing -> Node (createSubtree xs : children) key isComplete

containsPrefix :: [Char] -> Trie -> Bool
containsPrefix word = isJust . findNode word

contains :: [Char] -> Trie -> Bool
contains word node = fromMaybe False (findNode word node >>= isCompleted)

createSubtree :: [Char] -> Trie
createSubtree [x] = Node [] x True
createSubtree (x : xs) = Node [createSubtree xs] x False

replaceChild :: Trie -> [Trie] -> [Trie]
replaceChild newChild@(Node _ key _) = map (\c@(Node _ childKey _) -> if childKey == key then newChild else c)

findChild :: Char -> [Trie] -> Maybe Trie
findChild key = find (\(Node _ childKey _) -> childKey == key)

findNode :: [Char] -> Trie -> Maybe Trie
findNode [] node = Just node
findNode (x : xs) node = findChild x (getChildren node) >>= findNode xs

getChildren :: Trie -> [Trie]
getChildren (Root children) = children
getChildren (Node children _ _) = children

isCompleted :: Trie -> Maybe Bool
isCompleted (Node _ _ isComplete) = Just isComplete
