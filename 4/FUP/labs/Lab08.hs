separate :: [Int] -> ([Int], [Int])
separate [] = ([], [])
separate [x] = ([x], [])
separate (x:y:xs) = let (evs, ods) = separate xs
                    in (x:evs, y:ods)

numToStr :: Int -> Int -> String
numToStr n radix = if  n < radix then [chars !! n]
                   else (numToStr d radix) ++ [chars !! r]
                 where chars = ['0'..'9'] ++ ['A'..'F']
                       d = n `div` radix
                       r = n `mod` radix

split :: Int -> [Int] -> [[Int]]
split n xs | (length xs) <= n = [xs]
           | otherwise = take n xs : (split n (drop n xs))

-- -- solution using splitAt :: Int -> [a] -> ([a], [a])
split2 :: Int -> [Int] -> [[Int]]
split2 n xs = case splitAt n xs of
               ([], _) -> []
               (a, b)  -> a : split2 n b

averageN :: Int -> [Int] -> [Float]
averageN n ys =
  [fromIntegral (sum xs) / fromIntegral (length xs) | xs <- xss]
    where xss = split n ys

copy :: Int -> String -> String
copy n str | n <= 0 = ""
           | otherwise = str ++ copy (n - 1) str
 
-- tail recursive version
copy2 :: Int -> String -> String
copy2 n str = iter n "" where
    iter k acc | k <= 0 = acc
               | otherwise = iter (k-1) (acc ++ str) 

luhnDouble :: Int -> Int
luhnDouble n | n > 4 = 2*n - 9
             | otherwise = 2*n
 
luhn :: [Int] -> Bool
luhn xs = (sum evs + sum [luhnDouble x | x <- ods]) `mod` 10 == 0
    where rxs = reverse xs
          (evs, ods) = separate rxs

main = do
    print (separate [1,2,3,4,5])
    print (numToStr 52 10)
    print (numToStr 5 2)
    print (numToStr 255 16)
    print (split 3 [1..10])
    print (split 3 [1,2])
    print (split2 3 [1..10])
    print (split2 3 [1,2])
    print (averageN 3 [-1,0,1,2,3])
    print (copy 3 "abc")
    print (copy2 3 "def")
    print (luhnDouble 3)
    print (luhnDouble 7)
    print (luhn [1,7,8,4])
    print (luhn [4,7,8,3])
