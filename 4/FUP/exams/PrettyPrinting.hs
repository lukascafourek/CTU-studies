type Img = [String]

empty :: Img
empty = replicate 4 ""

zero :: Img
zero = [".##.",
        "#..#",
        "#..#",
        ".##."]

one :: Img
one =  ["...#",
        "..##",
        "...#",
        "...#"]

zipTwo :: Img -> Img -> Img
zipTwo = zipWith (\x y -> x ++ "." ++ y)

zipImgs :: [Img] -> Img
zipImgs (image:images) = foldl zipTwo image images
zipImgs _ = empty

toStr :: Int -> Int -> String
toStr num bin = if num < bin then [chars !! num]
                  else toStr d bin ++ [chars !! m] 
                    where chars = ['0'..'9'] ++ ['A'..'F']
                          d = num `div` bin
                          m = num `mod` bin

makeImgs :: String -> [Img]
makeImgs = map (\c -> case c of
                        '0' -> zero
                        '1' -> one)
        
main :: IO ()
main = do putStrLn "Enter integer:"
          num <- read <$> getLine
          let binNum = toStr num 2
          let images = makeImgs binNum
          mapM_ putStrLn $ zipImgs images
