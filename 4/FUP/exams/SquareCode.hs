module SquareCode ( encode ) where
import Data.Char ( isAlpha, toLower )
import Data.List ( transpose )

strMat :: String -> String
strMat str = [toLower ch | ch <- str, isAlpha ch]

numOfCols :: Int -> Int
numOfCols n = ceiling $ sqrt $ fromIntegral n

makeRows :: String -> Int -> [String]
makeRows str n | str == "" = []
               | len < n = [str ++ replicate (n - len) ' ']
               | otherwise = take n str:makeRows (drop n str) n
               where len = length str

encode :: String -> String
encode str = unwords trs
            where mat = strMat str
                  n = numOfCols $ length mat
                  rs = makeRows mat n
                  trs = transpose rs
