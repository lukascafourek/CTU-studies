module BuildingTrees (addEdge, buildTree, Tree(..)) where
import Data.List ( sortOn )

data Tree a = Leaf { val :: a }
            | Node { val :: a,
                     children :: [Tree a] } deriving (Eq, Show)

type Edge a = (a,a)

addEdge :: Ord a => Tree a -> Edge a -> Tree a
addEdge (Leaf x) (from, to) | x == from = Node x [Leaf to]
                            | otherwise = Leaf x
addEdge (Node x children) (from, to) | x == from = Node x (sortOn val (Leaf to:children))
                                     | otherwise = Node x [addEdge next (from, to) | next <- children]

buildTree :: Ord a => Tree a -> [Edge a] -> Tree a
buildTree = foldl addEdge 
