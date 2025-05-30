module Knights (is_valid) where
data Piece = Nil | Knight deriving Show

enum :: [a] -> [(Int, a)]
enum = zip [0..]

add :: Int -> [(Int, Piece)] -> [(Int, Int, Piece)]
add x = map (\(y, z) -> (x, y, z))

isKnight :: Piece -> Bool
isKnight Knight = True
isKnight Nil = False

knightCoords :: [[Piece]] -> [(Int, Int)]
knightCoords board = map (\(i, j, _) -> (i, j)) knights where
    knights = concatMap (filter (\(_, _, k) -> isKnight k)) cds
    cds = map (uncurry add) (enum (map enum board))

isValidPair :: (Int, Int) -> (Int, Int) -> Bool
isValidPair (a, b) (c, d) | abs (a - c) == 1 && abs (b - d) == 2 = False
                          | abs (a - c) == 2 && abs (b - d) == 1 = False
                          | otherwise = True

is_valid :: [[Piece]] -> Bool
is_valid board = all (uncurry isValidPair) [(x, y) | x <- cd, y <- cd] where
    cd = knightCoords board
