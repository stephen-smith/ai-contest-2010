-- | Example bot using the PlanetWars module
--
module Main where

import Data.List (maximumBy, minimumBy, partition, sortBy)
import qualified Data.IntMap as IM
import Data.Ord (comparing)

import PlanetWars

doTurn :: GameState  -- ^ Game state
       -> [Order]    -- ^ Orders
doTurn state = if null myPlanets
    -- No valid orders
    then []
    -- If we have a fleet in flight, just do nothing
    else attackOrders targets availableShips
  where
    (myFleets, enemyFleets) = partition isAllied $ gameStateFleets state

    -- Partition all planets
    (myPlanets, notMyPlanets) = partition isAllied $
        map snd $ IM.toList $ gameStatePlanets state

    -- Longest trip from one of my planets
    maxDistance p = maximum $ map (distanceBetween p) myPlanets

    -- Count the number of ships in all given fleets headed toward a planet.
    sumFleetShips fs p = sum $ map fleetShips
        $ filter ((== planetId p) . fleetDestination) fs

    -- Ships available to send (~60%)
    availableShips = total
      where
        total = sum $ map planetAvailable myPlanets
        planetAvailable p = maximum [0, planetShips p - sumFleetShips enemyFleets p]

    extendPlanet p = (p, (distance, ships))
      where
        distance = ceiling $ maxDistance p
        ships = (if isHostile p then planetGrowthRate p * distance else 0)
              + planetShips p
              - sumFleetShips myFleets p

    -- Heuristic value of a planet.
    -- Planets with high production are preferred.
    --   Enemy planets have their production doubled, because it takes
    --   away from the opposition.
    -- In case of tie, use the closet one.
    -- In case of tie, use the one that takes the fewest ships to conquer
    value (p, (d, s)) = (-growth, d, s)
      where
        growth = (if isHostile p then (*2) else id) $ planetGrowthRate p

    -- Target the "best" planet that we have enough ships to take.
    targets = filter ((< availableShips) . snd . snd)
           $ sortBy (comparing value) $ map extendPlanet notMyPlanets
    target = head targets

    attackOrders []  _ = []
    attackOrders  _  0 = []
    attackOrders ts as = orders ++ attackOrders ts' as'
      where
        -- Choose the target, retain the rest.
        (target:remaining) = ts

        -- Send at least enough ships to conquer it.
        totalFleetSize = (snd $ snd target) + 1

        -- Per-planet fleet size
        fleetSize p = (totalFleetSize * planetShips p) `divCeil` as
          where
            divCeil x y = if r > 0 then q + 1 else q
              where (q, r) = x `divMod` y

        -- Calulate the first set of orders
        order p = Order (planetId p) (planetId $ fst target) (fleetSize p)
        orders = filter ((> 0) . orderShips) $ map order myPlanets

        -- Reduce targets and availableShips for recursive call
        as' = as - (sum $ map orderShips orders)
        ts' = filter ((< as') . snd . snd) $ remaining

main :: IO ()
main = bot doTurn
