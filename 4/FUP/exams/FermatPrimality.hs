import Control.Monad.State

data LCG = LCG  Int Int Int Int deriving Show

generate :: State LCG Int
generate = do
    (LCG a x c m) <- get
    let x' = (a*x + c) `mod` m
    put (LCG a x' c m)
    return x'

generate_range :: Int -> Int -> State LCG Int
generate_range upp low = do
                x <- generate
                let x' = (x `mod` (low  - upp)) + upp
                return x'

mod_power :: Int -> Int -> Int -> Int
mod_power a k m = recur a k where
    recur b 1 = b
    recur b n = recur (b*a `mod` m) (n - 1)

fermat :: Int -> Int -> Int
fermat p b = mod_power b (p - 1) p

check_fermat :: Int -> Int -> Int -> State LCG Bool
check_fermat p n 1 = primality p (n - 1)
check_fermat _ _ _ = return False

primality :: Int -> Int -> State LCG Bool
primality p 0 = return True
primality p n = do 
                b <- generate_range 1 p
                check_fermat p n (fermat p b)
