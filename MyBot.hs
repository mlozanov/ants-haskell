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
directionTo x y
  | fst x > fst y = North
  | fst x < fst y = South
  | snd x > snd y = West
  | otherwise = East
  

-- | Generates orders for an Ant in all direction

type TakenPositions = Set.Set Point

isValidOrder :: TakenPositions -> World -> Order -> Bool
isValidOrder tp w o = 
  let newPoint = move (directionOrder o) (pointAnt $ ant o)
  in (passable w o)  && (not $ (Set.member newPoint tp))

generateOrders :: [Ant] -> TakenPositions -> World -> GameParams -> GameState -> [Order]  
generateOrders [] _ _ _ _ = []
generateOrders (a : xs) tp w gp gs = (generateOrders xs newTp w gp gs) ++ [tempOrder]
  where food = foodTiles gs
        antPos = pointAnt a
        closestFood = closestPoint gp food 435246523 antPos (0,0)
        tempOrder = (Order a) $ directionTo antPos (head $ pathFind gp gs (viewradius2 gp) closestFood a) --closestFood
        newTp = if isValidOrder tp w tempOrder
          then Set.insert (move (directionOrder tempOrder) (pointAnt $ a)) tp
          else tp
        
closestPoint :: GameParams -> [Point] -> Int -> Point -> Point -> Point
closestPoint _ [] _ _ p = p 
closestPoint gp (f:fs) d ap p 
  | newDist <= d = closestPoint gp fs d' ap f
  | otherwise = closestPoint gp fs d' ap p
    where d' = min newDist d
          newDist = distance gp ap f
                         
--buildGraph :: [Point] -> Ant -> Int -> Graph.Graph
--buildGraph (f:fs) a i = Graph.buildG (0,10::Graph.Vertex) []

pathFind :: GameParams -> GameState -> Int -> Point -> Ant -> [Point]
pathFind _ _ 0 _ _ = []
pathFind gp gs radius target ant = [(fst $ findMinDist ds (head ds))] ++ pp
    where antPos = pointAnt ant

          w = world gs

          possiblePaths = map (fm antPos) possibleDirections
              where possibleDirections = filter (\dir -> (passable w (Order ant dir)) && (unoccupied w (Order ant dir))) [North .. West]
                    fm = flip move

          ds = map (\p -> (p, distance gp p target)) possiblePaths

          findMinDist :: [(Point, Int)] -> (Point, Int) -> (Point, Int)
          findMinDist [] (mp, md) = (mp, md)
          findMinDist ((p,d):ps) (mp, md) 
              | md < d = findMinDist ps (mp, md)
              | otherwise = findMinDist ps (p, d)

          pp = pathFind gp gs (radius - 1) target ant


doTurn :: GameParams -> GameState -> IO [Order]
doTurn gp gs = do
  -- generate orders for all ants belonging to me
    -- 
  let mine = myAnts $ ants gs  
      generatedOrders = generateOrders mine (Set.empty) (world gs) gp gs

  -- this shows how to check the remaining time
  --  elapsedTime <- timeRemaining gs
  --  hPutStrLn stderr $ show elapsedTime
  -- wrap list of orders back into a monad
  return generatedOrders
-- | This runs the game
main :: IO ()
main = game doTurn

-- vim: set expandtab:
