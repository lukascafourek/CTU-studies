import Control.Monad.State
data Transition a b = Tr a b a deriving Show
data Automaton a b = NFA [Transition a b] a [a] deriving Show

nfa::Automaton Int Char
nfa = NFA [Tr 1 'a' 2,
           Tr 2 'b' 2,
           Tr 1 'a' 3,
           Tr 3 'b' 4,
           Tr 4 'a' 3,
           Tr 2 'a' 4]
           1
           [2,3]

walk :: (Eq a, Eq b) => Automaton a b -> a -> [b] -> Bool
walk aut@(NFA trs start finals) state [] | state `elem` finals = True
                                         | otherwise = False
walk aut@(NFA trs start finals) state (c:ws) = or [ walk aut t ws | tr@(Tr f c' t) <- trs, c' == c, f == state]

accepts :: (Eq a, Eq b) => Automaton a b -> [b] -> Bool
accepts aut@(NFA trs start finals) = walk aut start

combinations :: [a] -> Int -> State [[a]] [[a]]
combinations cs 0 = do get
combinations cs n = do ws <- get
                       let ws' = [ c:w | c <- cs , w <- ws]
                       put ws'
                       combinations cs (n-1)

lwords :: (Eq a, Eq b) => [b] -> Automaton a b -> Int -> [[b]]
lwords abc aut n = reverse $ [ w |  w <- p, accepts aut w] where
    p = evalState (combinations abc n) [[]]
