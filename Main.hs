-- | Example bot using the PlanetWars module
--
module Main where

import Data.List (maximumBy, minimumBy, partition, sortBy)
import qualified Data.IntMap as IM
import Data.Ord (comparing)

import PlanetWars

doTurn :: GameState  -- ^ Game state
       -> [Order]    -- ^ Orders
doTurn state = if null myPlanets || null targets
    -- No valid orders
    then []
    -- If we have a fleet in flight, just do nothing
    else orders
  where
    myFleets = filter isAllied $ gameStateFleets state

    -- Partition all planets
    (myPlanets, notMyPlanets) = partition isAllied $
        map snd $ IM.toList $ gameStatePlanets state

    -- Longest trip from one of my planets
    maxDistance p = maximum $ map (distanceBetween p) myPlanets

    -- Ships available to send
    availableShips = sum $ map planetShips myPlanets

    -- Heuristic value of a planet.
    -- Planets with high production are preferred.
    -- In case of tie, use the closet one.
    -- In case of tie, use the one that takes the fewest ships to conquer
    value p = (-growth, distance, ships)
      where
        growth = planetGrowthRate p
        distance = maxDistance p
        ships = planetShips p

    -- Target the "best" planet that we have enough ships to take.
    targets = filter ((< availableShips) . planetShips)
           $ sortBy (comparing value) notMyPlanets
    target = head targets

    -- Send at least enough ships to conquer it.
    totalFleetSize = planetShips target + 1

    -- Per-planet fleet size
    fleetSize p = (totalFleetSize * planetShips p) `divCeil` availableShips
      where
        divCeil x y = if r > 0 then q + 1 else q
          where (q, r) = x `divMod` y

    -- Per-planet orders
    order p = Order (planetId p) (planetId target) (fleetSize p)

    -- All orders
    orders = filter ((> 0) . orderShips) $ map order myPlanets

main :: IO ()
main = bot doTurn
