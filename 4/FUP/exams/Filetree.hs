module Filetree (parse, exists) where
import Data.Maybe ( fromMaybe )
import Data.Map (Map)
import qualified Data.Map as Map
import FTree ( FTree(FNode, FNil) )

files =  ["src/tree.hs"
         ,"src/complex.hs"
         ,"scripts/ex1/test.ss"
         ,"scripts/ex1/eval.ss"
         ,"scripts/emptydir"
         ,"scripts/ex2/test.ss"
         ,"tests/test_tree.hs"]

split :: Char -> String -> [String]
split slash file = case break (== slash) file of
                    (l, "") -> [l]
                    (l, r) -> l : split slash (drop 1 r)

insert :: [String] -> FTree String -> FTree String
insert [] tree = tree
insert (l:r) FNil = FNode (Map.insert l (insert r FNil) Map.empty)
insert (l:r) (FNode lst) = FNode (Map.alter (Just . insert r . fromMaybe FNil) l lst)

parse :: [String] -> FTree String
parse = foldl (\tree file -> insert (split '/' file) tree) FNil

exists :: String -> FTree String -> Bool
exists file = _exists (split '/' file) where
    _exists [] tree = True
    _exists _ FNil = False
    _exists (l:r) (FNode lst) = maybe False (_exists r) (Map.lookup l lst)
