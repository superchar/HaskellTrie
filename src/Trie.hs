module Trie (put, Trie (..)) where

import Data.List

data Trie = Root [Trie] | Node Char Bool [Trie] deriving (Show)

put :: Trie -> [Char] -> Trie
put (Root children) word@(x : xs) = case findChild x children of
  Just child -> Root (replaceChild (put child xs) children)
  Nothing -> Root (createSubtree word : children)
put (Node key isComplete children) [] = Node key True children
put (Node key isComplete children) (x : xs) = case findChild x children of
  Just child -> Node key isComplete (replaceChild (put child xs) children)
  Nothing -> Node key isComplete (createSubtree xs : children)

createSubtree :: [Char] -> Trie
createSubtree [x] = Node x True []
createSubtree (x : xs) = Node x False [createSubtree xs]

replaceChild :: Trie -> [Trie] -> [Trie]
replaceChild newChild@(Node key _ _) = map (\c@(Node childKey _ _) -> if childKey == key then newChild else c)

findChild :: Char -> [Trie] -> Maybe Trie
findChild key = find (\(Node childKey _ _) -> childKey == key)
