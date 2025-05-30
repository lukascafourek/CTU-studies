module BalancedTree ( Tree (..), buildHeap) where

data Tree a = Leaf | Node a (Tree a) (Tree a)

tostr :: (Show a) => Tree a -> Int -> String
tostr Leaf d = ""
tostr (Node x l r) d = tostr l (d+1) ++ concat (replicate d "---") ++ show x ++ "\n" ++  tostr r (d+1)
instance (Show a) => Show (Tree a) where
    show :: Show a => Tree a -> String
    show tree = tostr tree 0

minDepth :: Tree a -> Int
minDepth Leaf = 0
minDepth (Node _ l r) = 1+min (minDepth l) (minDepth r)

enforce :: (Eq a, Ord a) => Tree a -> Tree a
enforce Leaf = Leaf
enforce tr@(Node e Leaf Leaf) = tr
enforce (Node e l Leaf) = Node nv (Node nlv ll lr) Leaf
    where (Node lv ll lr) = enforce l
          nlv = min e lv
          nv = max lv e
enforce (Node e l r) = Node nv (Node nlv ll lr) (Node nrv rl rr)
    where (Node lv ll lr) = enforce l
          (Node rv rl rr) = enforce r
          nlv = min e lv
          nrv = min e rv
          nv = max (max lv rv) e

insert :: (Eq a, Ord a) => a -> Tree a -> Tree a
insert e Leaf = Node e Leaf Leaf
insert e tree@(Node v l r) = dfs tree
    where dfs Leaf = Node e Leaf Leaf
          dfs tr@(Node n l r) = if minDepth r < minDepth l then Node n l (dfs r)
                                  else Node n (dfs l) r

buildHeap :: (Eq a, Ord a) => [a] -> Tree a
buildHeap [] = Leaf
buildHeap (e:el) = enforce ( insert e (buildHeap el) )
