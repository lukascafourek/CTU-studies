module SpiralMatrix ( spiralMatrix ) where
type Matrix = [[Int]]

add small n = (map . map) (+ n) small

wrap mat _first _last = _first : mat ++ [_last]

spiralMatrix :: Int -> Matrix
spiralMatrix 1 = [[1]]
spiralMatrix n = if even n then [[]] else
    vertical $ horizontal $ add small (4*n - 4) where
    small = spiralMatrix (n - 2)
    horizontal mat = zipWith3 wrap mat [4*n - 4, 4*n - 5..3*n - 1] [n + 1 .. 2*n + 2]
    vertical mat = wrap mat [1..n] [3*n - 2, 3*n - 3 .. 2*n - 1]
