module SpanningTree (minSpanningTree, Graph (..), Edge (..)) where
import Data.List -- for sortOn

data Edge a b = Edge { u :: a,
                       v :: a,
                       weight :: b } deriving (Eq,Show)

data Graph a b = Graph { nodes :: [a],
                         edges :: [Edge a b] } deriving Show  

gr2 :: Graph Char Int
gr2 = Graph{ nodes = ['A'..'F'],
             edges = [Edge 'A' 'B' 1,
                      Edge 'D' 'E' 4,
                      Edge 'E' 'F' 7,
                      Edge 'A' 'D' 5,
                      Edge 'B' 'E' 2,
                      Edge 'C' 'F' 5,
                      Edge 'D' 'B' 6,
                      Edge 'E' 'C' 4,
                      Edge 'A' 'E' 3] }

reverseEdge :: Edge a b -> Edge a b
reverseEdge edge = edge{ u=v edge, v=u edge}

reverseEdges :: Graph a b -> Graph a b
reverseEdges graph@Graph{ edges=egs } = graph{ edges=founds }
                         where founds = egs ++ map reverseEdge egs

findEdge :: (Eq a, Ord b) => [Edge a b] -> [a] -> [a] -> Edge a b
findEdge egs covered uncovered = head active 
                  where active = sortOn weight [ edge | edge <- egs, u edge `elem` covered, v edge `elem` uncovered ]

jarnik :: (Eq a, Ord b) => Graph a b -> [a] -> [a] -> [Edge a b] -> [Edge a b]
jarnik _ _ [] egs = egs
jarnik graph covered uncovered egs = jarnik graph (node:covered) (delete node uncovered) (found:egs)
                       where found = findEdge (edges graph) covered uncovered
                             node = v found

minSpanningTree :: (Eq a, Ord b) => Graph a b -> [Edge a b]
minSpanningTree graph = jarnik _graph [node] xs []
         where _graph = reverseEdges graph 
               (node:xs) = nodes graph
