module Main where

import Data.List
import qualified Data.Set as Set
import qualified Data.Graph as Graph
import Data.Maybe (mapMaybe, catMaybes)
import System.IO

import Ants

-- | Picks the first "passable" order in a list
-- returns Nothing if no such order exists
tryOrder :: World -> [Order] -> Maybe Order
tryOrder w = find (passable w)


--- Utility functions
foodTiles :: GameState -> [Point]
foodTiles gs = food gs

directionTo :: Point -> Point -> Direction
directionTo a b
  | fst a > fst b = East
  | fst a < fst b = West
  | snd a > snd b = North
  | otherwise = South
  

-- | Generates orders for an Ant in all direction

type TakenPositions = Set.Set Point

isValidOrder :: TakenPositions -> World -> Order -> Bool
isValidOrder tp w o = 
  let newPoint = move (direction o) (point $ ant o)
  in (passable w o) && (not $ (Set.member newPoint tp))

generateOrders :: [Ant] -> TakenPositions -> World -> GameParams -> GameState -> [Order]  
generateOrders [] _ _ _ _ = []
generateOrders (a : xs) tp w gp gs = (generateOrders xs newTp w gp gs) ++ [tempOrder]
  where food = foodTiles gs
        antPos = point a
        tempOrder = (Order a) (directionTo antPos (closestPoint gp food (distance gp antPos (head food)) antPos (0,0)))
        newTp = if isValidOrder tp w tempOrder
          then Set.insert (move (direction tempOrder) (point $ a)) tp
          else tp
        
closestPoint :: GameParams -> [Point] -> Int -> Point -> Point -> Point
closestPoint gp [] d ap p = p 
closestPoint gp (f:fs) d ap p = closestPoint gp fs d' ap p'
    where d' = min (distance gp ap f) d
          p' = if (d' <= d) then f else p
                         
--buildGraph :: [Point] -> Ant -> Int -> Graph.Graph
--buildGraph (f:fs) a i = Graph.buildG (0,10::Graph.Vertex) []

{- | 
 - Implement this function to create orders.
 - It uses the IO Monad so algorithms can call timeRemaining.
 -
 - GameParams data holds values that are constant throughout the game
 - GameState holds data that changes between each turn
 - for each see Ants module for more information
 -}
doTurn :: GameParams -> GameState -> IO [Order]
doTurn gp gs = do
  -- generate orders for all ants belonging to me
    -- 
  let mine = myAnts $ ants gs
      generatedOrders = generateOrders mine (Set.empty) (world gs) gp gs
  -- for each ant take the first "passable" order, if one exists
  --    orders = mapMaybe (tryOrder (world gs)) generatedOrders
  -- this shows how to check the remaining time

  elapsedTime <- timeRemaining gs
  hPutStrLn stderr $ show elapsedTime
  -- wrap list of orders back into a monad
  return generatedOrders
-- | This runs the game
main :: IO ()
main = game doTurn

-- vim: set expandtab:
