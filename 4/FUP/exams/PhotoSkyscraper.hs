module PhotoSkyscraper (bestView) where
import Data.List ( group, sortOn, transpose )

city :: [[Int]]
city = [[3, 3, 3],
        [1, 2, 3],
        [1, 2, 3]]

roofs :: (Foldable t, Functor t, Ord a) => t [a] -> Int
roofs matrix = sum $ chooseMax <$> matrix where
    chooseMax mat = length (group $ scanl1 max mat)

change :: Char -> [[a]] -> [[a]]
change 'N' = transpose
change 'S' = fmap reverse . transpose
change 'E' = fmap reverse
change _ = id

bestView :: [[Int]] -> (Char, Int)
bestView city = 
  let directions = "NSEW"
      views = roofs . (`change` city) <$> directions
      zipped = zip directions views
  in last $ sortOn snd zipped
