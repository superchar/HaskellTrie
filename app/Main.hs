module Main (main) where

import Lib
import Trie

main :: IO ()
main = print (put (put (Root []) ['T', 'e', 's', 't', 'i', 'n', 'g']) ['T', 'e', 's', 't'])
                    
 