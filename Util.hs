module Util
  ( fAnd 
  , fOr
  , tuplify2
  , bfs
  , Vertex
  , Edge
  ) where

import List

fAnd :: a -> [a -> Bool] -> Bool
fAnd x = all ($x)

fOr :: a -> [a -> Bool] -> Bool
fOr x = any ($x)

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

type Coord = (Integer, Integer)
type Vertex = Coord             
type Edge = (Coord, Coord)

test = [((1,2), (3,4)), ((1,2), (5,6)), ((3,4), (7,8)), ((1,2), (7,8)), ((3,4), (9,10)),((9,10), (11,12))]

--This function is bidirectional (i.e, if V is connected to V', it is assumed that V' is connected to V)            --Doesn't work for directional graphs, but we shouldn't care about that
            
isAdjacent :: [Vertex] ->  Vertex -> Edge -> Bool
isAdjacent visited v (x,y)
  | x == v = not $ y `elem` visited
  | y == v = not $ x `elem` visited
  | otherwise = False
                
findNeighbours :: [Vertex] -> [Edge] -> Vertex -> [Vertex]
findNeighbours visited g s = map (\(x,y) -> if x == s then y else x ) [x | x <- g, isAdjacent visited s x]

enqueueMany :: [[Vertex]] -> [Vertex] -> [Vertex] -> [[Vertex]]
enqueueMany q _ [] = q               
enqueueMany q path (v : vs) = enqueueMany (q ++ [(path ++ [v])]) path vs

bfs :: Vertex -> Vertex -> [Edge] -> [Vertex]
bfs source dest g = bfs' dest g [[source]] [source]
  
bfs' :: Vertex -> [Edge] -> [[Vertex]] -> [Vertex] -> [Vertex]
bfs' _ _ [] visited = []                              
bfs' dest g q visited
  | l_node == dest = t_path
  | otherwise = bfs' dest g new_q new_visited
  where t_path = head q
        l_node = last t_path
        neighbours = findNeighbours visited g l_node
        new_q = enqueueMany (tail q) t_path neighbours
        new_visited = visited ++ neighbours
          
          
