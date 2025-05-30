interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : [y:xs | xs <- interleave x ys]

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concat [interleave x p | p <- permutations xs]

type Edge a = (a,a) 
data Graph a = Graph {vertices :: [a], edges :: [Edge a]} deriving Show

gr :: Graph Int
gr = Graph {vertices=[1..6], edges=[(1, 2), (1, 5), (2, 3), (2, 5), (3, 4), (4, 5), (4, 6)]}

isEdge :: Eq a => Edge a -> Graph a -> Bool
isEdge (a,b) g = (a,b) `elem` edgs || (b,a) `elem` edgs where
    edgs = edges g

isPath :: Eq a => [a] -> Graph a -> Bool
isPath vs g = and [ isEdge pair g | pair <- zip vs (tail vs) ]

findHamiltonian :: Eq a => Graph a -> [[a]]
findHamiltonian g = [p | p <- perms, isPath p g]
    where perms = permutations (vertices g)

data DualNum a = DN a a deriving (Eq, Ord)

instance Show a => Show (DualNum a) where
    show (DN x x') = show x ++ " + " ++ show x' ++ "eps"

instance Num a => Num (DualNum a) where
    (DN x x') + (DN y y') = DN (x + y) (x' + y')
    (DN x x') - (DN y y') = DN (x - y) (x' - y')
    (DN x x') * (DN y y') = DN (x * y) (x*y' + y*x')
    fromInteger i = DN (fromInteger i) 0
    abs (DN x x') = DN (abs x) (signum x * x')
    signum (DN x _) = DN (signum x) 0

instance Fractional a => Fractional (DualNum a) where
    (DN x x') / (DN y y') = DN (x/y) ((x'*y - x*y') / (y*y))
    fromRational r = DN (fromRational r) 0

f :: Num a => a -> a
f x = x^2 + 1

g :: Fractional a => a -> a
g x = (x^2 - 2) / (x - 1)

sqr :: (Fractional a, Ord a) => a -> a
sqr x = convAbs $ iterate improve 1
  where improve r = (r + x/r) / 2
        convAbs (x1:x2:xs) | abs (x1-x2) < 1e-10 = x2
                           | otherwise = convAbs xs

instance (Floating a) => Floating (DualNum a) where
    pi               = DN pi 0
    exp (DN x x')    = DN r (r*x') where r = exp x 
    log (DN x x')    = DN (log x) (x' / x)
    sqrt (DN x x')   = DN r (x' / (2 * r)) where r = sqrt x
    sin (DN x x')    = DN (sin x) (x' * cos x) 
    cos (DN x x')    = DN (cos x) (-x' * sin x) 
    acos (DN x x')   = DN (acos x) (-x' / sqrt(1 - x*x))
    asin (DN x x')   = DN (asin x) ( x' / sqrt(1 - x*x))
    atan (DN x x')   = DN (atan x) ( x' / (1 + x*x))
    sinh x           = (exp x - exp (-x)) / 2
    cosh x           = (exp x + exp (-x)) / 2
    asinh x          = log (x + sqrt (x*x + 1))
    acosh x          = log (x + sqrt (x*x - 1))
    atanh x          = (log (1 + x) - log (1 - x)) / 2

f2 :: Floating a => a -> a
f2 x = x^4 + sin (x^2) - exp x * log x + 7

-- the derivative of f2
df2 :: Floating a => a -> a
df2 x = 4*x^3 + 2 * x * cos (x^2) - exp x/x - exp x * log x

merge :: (a -> Int) -> [a] -> [a] -> [a]
merge _ [] ys = ys
merge _ xs [] = xs
merge f p@(x:xs) q@(y:ys) | f x < f y = x:merge f xs q
                          | otherwise = y:merge f p ys
 
subseqs :: [a] -> [[a]]
subseqs [] = [[]]
subseqs (x:xs) = merge length (subseqs xs) [x:ys | ys <- subseqs xs]

main = do
    print (interleave 0 [1,2,3])
    print gr
    print (vertices gr)
    print (edges gr)
    print (f (DN 5 1))
    print (g (DN 0 1))
    print (sqr (DN 9 1))
    print (f2 (DN 3 1))
    print (df2 3)
