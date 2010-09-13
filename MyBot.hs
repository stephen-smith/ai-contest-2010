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
    else attackOrders modPlanets targets
  where
    (myFleets, enemyFleets) = partition isAllied $ gameStateFleets state

    -- Partition all planets
    planetsMap = gameStatePlanets state
    (myPlanets, notMyPlanets) = partition isAllied $
        map snd $ IM.toList planetsMap

    planetById id = planetsMap IM.! id

    -- Cache distance calculations.
    distances :: IM.IntMap (IM.IntMap Int)
    distances = IM.map oneToMany planetsMap
      where
        oneToMany p = IM.map (turnsBetween p) planetsMap
        turnsBetween p q = ceiling $ distanceBetween p q

    distanceById x y = (distances IM.! x) IM.! y
    distance p q = distanceById (planetId p) (planetId q)

    -- Calculate way-points
    waypoints = IM.mapWithKey oneToMany planetsMap
      where
        oneToMany pid _ = IM.mapWithKey (allWaypoints pid) planetsMap
        allWaypoints pid qid _ = IM.elems $ IM.filterWithKey isWaypoint planetsMap
          where
            dz = distanceById pid qid
            isWaypoint rid _ = pid /= rid && rid /= qid && dx + dy <= dz
              where
                dx = distanceById pid rid
                dy = distanceById rid qid

    -- Use friendly way-points.
    orderViaWaypoints s d z = if null wps
        then Order s d z
        else Order s wp z
      where
        wps = sortBy (comparing $ distanceById s . planetId) $ filter isAllied
            $ (waypoints IM.! s) IM.! d
        wp = planetId $ head wps

    -- Count attackers
    attackingFleets = filter (isAllied . planetById .fleetDestination) enemyFleets

    attackersByDest = foldr accum IM.empty attackingFleets
      where
        accum f = IM.insertWith (+) (fleetDestination f) (fleetShips f)

    -- Hold back ships to defend
    modPlanets = map mut myPlanets
      where
        mut p = p { planetShips = r }
          where
            attackers = IM.findWithDefault 0 (planetId p) attackersByDest
            r = maximum [0, planetShips p - attackers]

    -- Longest trip from one of my planets
    maxDistance p = maximum $ map (distance p) myPlanets

    -- Count the number of ships in all given fleets headed toward a planet.
    sumFleetShips fs p = sum $ map fleetShips
        $ filter ((== planetId p) . fleetDestination) fs

    extendPlanet p = (p, (distance, ships))
      where
        distance = maxDistance p
        ships = maximum [0, ships']
        ships' =(if isHostile p then planetGrowthRate p * distance else 0)
              + planetShips p + 1
              - sumFleetShips myFleets p

    -- Heuristic value of a planet.
    -- Planets with high production are preferred.
    --   Enemy planets have their production doubled, because it takes
    --   away from the opposition.
    -- In case of tie, use the closet one.
    -- In case of tie, use the one that takes the fewest ships to conquer
    value (p, (d, s)) = (-growth, d, s)
      where
        growth = (if isHostile p then (+1) . (*2) else id) $ planetGrowthRate p

    -- Target the "best" planet that we have enough ships to take.
    targets = sortBy (comparing value) $ map extendPlanet notMyPlanets

    attackOrders mp ts | null targets        = []
                       | availableShips == 0 = []
                       | otherwise = orders ++ attackOrders mp' ts'
      where
        -- Ships available to send
        availableShips = total
          where total = sum $ map planetShips mp

        -- Choose the target, retain the rest.
        targets = filter ((< availableShips) . snd . snd) ts
        (target:ts') = targets

        -- Send at least enough ships to conquer it.
        totalFleetSize = (snd $ snd target)

        -- Per-planet fleet size
        fleetSize p = (totalFleetSize * planetShips p) `divCeil` availableShips
          where
            divCeil x y = if r > 0 then q + 1 else q
              where (q, r) = x `divMod` y

        -- Calulate the first set of orders
        delta p = (order, p')
          where
            fleet = fleetSize p
            order = orderViaWaypoints (planetId p) (planetId $ fst target) fleet
            p' = p { planetShips = planetShips p - fleet }
        (all_orders, mp') = unzip $ map delta mp
        orders = filter ((> 0) . orderShips) all_orders

main :: IO ()
main = bot doTurn
