module Main (main) where

import Trie

main :: IO ()
main = print (containsPrefix ['T', 'e', 's'] trie)
  where trie = put ['T', 'e', 's', 't'] (put ['T', 'e', 's', 't', 'i', 'n', 'g'] (Root []))
 