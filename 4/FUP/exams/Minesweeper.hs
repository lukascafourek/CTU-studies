import Data.Char ( intToDigit )
import Control.Monad ( replicateM )

cut :: Int -> Int -> [a] -> [a]
cut prev next board = drop prev $ take next board

neighbors :: [String] -> Int -> Int -> String
neighbors board x y = cut (y - 1) (y + 2) board >>= cut (x - 1) (x + 2)

sweep :: [String] -> [String]
sweep board = output out board where
    output f = zipWith (\y -> zipWith (\x e -> f e x y) [0..]) [0..]
    out e x y = final (mines x y) e
    final _ '*' = '*'
    final 0 _ = '.'
    final n _ = intToDigit n
    mines x y = count (neighbors board x y)
    count xs = length $ filter (== '*') xs

readInput :: IO [String]
readInput = do
    rows <- readLn
    replicateM rows getLine

main = do
  lines <- readInput
  putStrLn "\nSweep Result:"
  let sw = sweep lines
  mapM_ putStrLn sw
  