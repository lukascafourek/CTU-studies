module CheapFlights (cheapflight,Node,Cost,Edge,Graph,Path) where
import Data.List

type Node = Int
type Cost = Float
type Edge = (Node,Node,Cost)
type Graph = ([Node],[Edge])
type Path = [Node]

nodes :: [Node]
nodes = [1..6]

edges :: [Edge]
edges = [(1,2,0.5), (1,3,1.0), (2,3,2.0), (2,5,1.0), (3,4,4.0), (4,5,1.0)]

graph :: Graph
graph = (nodes,edges)

nextPos :: Node -> Graph -> [(Node,Cost)]
nextPos n g = [(y,z) | (x,y,z) <- snd g, x == n] ++ [(x,z) | (x,y,z) <- snd g, y == n]

extend :: (Path,Cost) -> Graph -> [(Path,Cost)]
extend ([],_) _ = []
extend (path@(p:_),c) m = map (\(n,c') -> (n:path,c+c')) $ nextPos p m

lowcost :: Ord b => (a,b) -> (a,b) -> Ordering
lowcost (_,x) (_,y) | x < y = LT
                    | otherwise = GT

bfs :: [Node] -> [(Path,Cost)] -> Node -> Graph -> Maybe (Path, Cost)
bfs _ [] _ _ = Nothing
bfs visited upaths q m
    | p == q = Just (reverse path, c)
    | p `elem` visited = bfs visited paths q m
    | otherwise = bfs (p:visited) (paths ++ extend (path,c) m) q m
    where ((path@(p:_),c):paths) = sortBy lowcost upaths

cheapflight :: Node -> Node -> Graph -> Maybe (Path,Cost)
cheapflight s = bfs [] [([s],0)]
