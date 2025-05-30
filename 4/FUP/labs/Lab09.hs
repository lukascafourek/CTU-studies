data Tree a = Leaf a | Node (Tree a) (Tree a)
instance (Show a) => Show (Tree a) where
    show (Leaf x) = "<Leaf " ++ show x ++ "/>"
    show (Node left right) = "<Node>" ++ show left ++ show right ++ "</Node>"

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) (Leaf 'd')

treeDepth :: Tree a -> Int
treeDepth (Leaf _) = 1
treeDepth (Node left right) = 1 + max (treeDepth left) (treeDepth right)

labelHlp :: Tree a -> Int -> (Tree (a, Int), Int)
labelHlp (Leaf x) n = (Leaf (x, n), n+1)
labelHlp (Node left right) n = let (left', n') = labelHlp left n
                                   (right', n'') = labelHlp right n'
                                in (Node left' right', n'')

labelTree :: Tree a -> Tree (a, Int)
labelTree t = fst (labelHlp t 0)

type Monomial a = (a, Int)
data Polynomial a = Null | Pol (Monomial a) (Polynomial a)
 
format :: (Show a, Num a, Ord a) => Monomial a -> String
format (c, e) | e == 0 = display c
              | otherwise = display c ++ "*x^" ++ show e 
    where display k | k >= 0 = show k
                    | otherwise = "(" ++ show k ++ ")"
 
instance (Show a, Num a, Ord a) => Show (Polynomial a) where
    show Null = "0"
    show (Pol m Null) = format m
    show (Pol m ms) = format m ++ " + " ++ show ms

getDegree :: Polynomial a -> Int
getDegree p = iter p (-1) where
    iter Null n = n
    iter (Pol (_, e) ms) n | e > n = iter ms e
                           | otherwise = iter ms n

main = do
    print tree
    print (treeDepth tree)
    print (labelTree tree)
    print Null
    print (Pol (3, 2) Null)
    print (Pol (-2, 0) Null)
    print (Pol (-1, 0) (Pol (-2, 1) (Pol (1, 3) Null)))
    print (getDegree Null)
    print (getDegree (Pol (3, 2) Null))
    print (getDegree (Pol (-2, 0) Null))
    print (getDegree (Pol (-1, 0) (Pol (-2, 1) (Pol (1, 3) Null))))
