module Ancestor (findPath, commonAncestor, Tree(..)) where

data Tree a = Leaf a
            | Node a (Tree a) (Tree a) deriving (Eq,Show)

tree = Node 1 (Node 2 (Leaf 5) (Leaf 6)) (Node 3 (Leaf 4) (Leaf 7))

tree2 = Node 1 (Node 2 (Leaf 3)
                       (Node 4 (Leaf 5)
                               (Leaf 6)))
               (Node 7 (Leaf 8)
                       (Leaf 9))

findPath :: Eq a => a -> Tree a -> [a]
findPath x (Leaf lf) | x == lf = [x]
                     | otherwise = []
findPath x (Node n l r) | x == n = [x]
                        | otherwise = let lchild = findPath x l
                                          rchild = findPath x r
                                      in if null (lchild ++ rchild) then []
                                         else n:(lchild ++ rchild)

commonAncestor :: Eq a => a -> a -> Tree a -> Maybe a
commonAncestor x y tr | null common = Nothing
                      | otherwise = Just $ last common 
                      where l = findPath x tr
                            r = findPath y tr
                            common = [x | (x, y) <- zip l r, x == y]
