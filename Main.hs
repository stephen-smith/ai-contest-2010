-- | Example bot using the PlanetWars module
--
module Main where

import Data.List (maximumBy, minimumBy, partition, sortBy)
import qualified Data.IntMap as IM
import Data.Ord (comparing)

import qualified System.IO.Unsafe (unsafePerformIO)

import PlanetWars

doTurn :: GameState  -- ^ Game state
       -> [Order]    -- ^ Orders
doTurn state = if null myPlanets
    -- No valid orders
    then []
    -- If we have a fleet in flight, just do nothing
    else attackOrders myPlanets targets
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
    targets = sortBy (comparing value) $ map extendPlanet notMyPlanets

    attackOrders mp ts | null targets = []
                       | otherwise    = orders ++ attackOrders mp' ts'
      where
        -- Ships available to send
        availableShips = sum $ map planetAvailable mp
          where
            planetAvailable p = maximum [0, planetShips p - incoming]
              where incoming = sumFleetShips enemyFleets p

        -- Choose the target, retain the rest.
        targets = filter ((< availableShips) . snd . snd) ts
        (target:ts') = targets

        -- Send at least enough ships to conquer it.
        totalFleetSize = (snd $ snd target) + 1

        -- Per-planet fleet size
        fleetSize p = (totalFleetSize * planetShips p) `divCeil` availableShips
          where
            divCeil x y = if r > 0 then q + 1 else q
              where (q, r) = x `divMod` y

        -- Calulate the first set of orders
        delta p = (order, p')
          where
            fleet = fleetSize p
            order = Order (planetId p) (planetId $ fst target) fleet
            p' = p { planetShips = planetShips p - fleet }
        (all_orders, mp') = unzip $ map delta mp
        orders = filter ((> 0) . orderShips) all_orders

main :: IO ()
main = bot doTurn
