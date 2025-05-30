module ConvexHull (convexHull) where
import Data.List ( sortBy )

points :: [(Double, Double)]
points = [(-2.0, 3.0), ( 2.0, 2.0), (-1.0, 1.0),
          (-2.0,-1.5), ( 4.0,-1.0), ( 1.0,-3.0)]

polarA :: RealFloat a => (a, a) -> (a, a) -> a
polarA (x1, y1) (x2, y2) = if r >= 0 then r else r + 2*pi where
                             r = atan2 (y2 - y1) (x2 - x1)

sortA :: RealFloat a => (a, a) -> [(a, a)] -> [(a, a)]
sortA point = sortBy f where
               f a b = if polarA point a > polarA point b then GT else LT

find :: (Ord a1, Ord a2) => [(a1, a2)] -> (a1, a2)
find (point:points) = foldl f point points where
                f (x1, y1) (x2, y2) | y1 == y2 = if x1 > x2 then (x1, y1) else (x2, y2)
                                    | otherwise = if y1 < y2 then (x1, y1) else (x2, y2)

isLeft :: (Ord a, Num a) => (a, a) -> (a, a) -> (a, a) -> Bool
isLeft a b c = cross a b c > 0 where
                   cross (x1, y1) (x2, y2) (x3, y3) = (x2-x1)*(y3-y1) - (y2-y1)*(x3-x1)

scan :: (Ord a, Num a) => [(a, a)] -> [(a, a)] -> [(a, a)]
scan (point:points) hull@(h1:h2:hs) = if isLeft h2 h1 point then graham points (point:hull) else graham (point:points) (h2:hs)

graham :: (Ord a, Num a) => [(a, a)] -> [(a, a)] -> [(a, a)]
graham [] hull = hull
graham (point:points) hull | length hull < 2 = graham points (point:hull)
                   | otherwise = scan (point:points) hull

convexHull :: RealFloat a => [(a, a)] -> [(a, a)]
convexHull points = reverse (graham sorted []) where
                    sorted = sortA (find points) points
