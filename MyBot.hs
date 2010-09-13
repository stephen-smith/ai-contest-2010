-- | Example bot using the PlanetWars module
--
module Main where

import Control.Applicative ((<$>), (<*>))

import Data.Function (on)
import Data.List (maximumBy, minimumBy, partition, sortBy, groupBy)
import qualified Data.IntMap as IM
import Data.Ord (comparing)

import qualified System.IO.Unsafe (unsafePerformIO)

import PlanetWars

splitWhile :: (a -> Bool) -> [a] -> ([a],[a])
splitWhile _ [] = ([], [])
splitWhile f l@(x:xs)
  | f x       = let (p, t) = splitWhile f xs in (x:p, t)
  | otherwise = ([], l)

doTurn :: GameState  -- ^ Game state
       -> [Order]    -- ^ Orders
doTurn state = if IM.null myPlanets
    -- No valid orders
    then []
    -- If we have a fleet in flight, just do nothing
    else attackOrders myPlanets targets
  where
    (myFleets, enemyFleets) = partition isAllied $ gameStateFleets state

    -- Partition all planets
    planetsMap = gameStatePlanets state
    (myPlanets, notMyPlanets') = IM.partition isAllied planetsMap
    notMyPlanets = IM.elems notMyPlanets'

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

    -- Fleet combining
    fleetByDest f = IM.singleton (fleetDestination f) (fleetShips f)
    attackersByDest = IM.unionsWith (+) $ map fleetByDest enemyFleets
    supportByDest = IM.unionsWith (+) $ map fleetByDest myFleets

    -- Longest trip from one of my planets
    maxDistance p = maximum $ map (distance p) $ IM.elems myPlanets

    -- Count the number of ships in all given fleets headed toward a planet.
    sumFleetShips fs p = sum $ map fleetShips
        $ filter ((== planetId p) . fleetDestination) fs

    extendPlanet p = (p, ((notMine, distance), ships))
      where
        distance = maxDistance p
        ships = maximum [0, ships']

        -- My ships
        myFlown = IM.findWithDefault 0 (planetId p) supportByDest
        myGrown = if isAllied p && theirFlown <= 0
            then planetGrowthRate p * distance
            else 0

        -- Not My Ships
        theirFlown = IM.findWithDefault 0 (planetId p) attackersByDest
        theirGrown = if isHostile p || theirFlown > 0
            then planetGrowthRate p * distance
            else 0

        notMine = isHostile p || theirFlown > 0

        -- Overcome defences
        overcomeOpposition = if isAllied p && theirFlown <= 0
            then 0
            else planetShips p + 1

        ships' = overcomeOpposition - (myGrown + myFlown)
               + (theirGrown + theirFlown)

    -- Heuristic value of a planet.
    value (p, ((nm, d), s)) = (s - growth, s, d)
      where
        turnsRemaining = 100
        growthFactor = if nm
            then growth * 2
            else growth
        growth = planetGrowthRate p * (turnsRemaining - d)

    -- Target the "best" planet that we have enough ships to take.
    targets = sortBy (comparing value) $ map extendPlanet $ IM.elems planetsMap

    attackOrders mp ts | null targets        = []
                       | availableShips == 0 = []
                       | otherwise = orders ++ attackOrders mp' ts'
      where
        mpl = IM.elems mp

        -- Ships available to send
        availableShips = total
          where total = sum $ map planetShips mpl

        -- Choose the target, retain the rest.
        (_, targets) = splitWhile ((> availableShips) . snd . snd) ts
        (target:ts') = targets

        -- Send at least enough ships to conquer it.
        totalFleetSize = (snd $ snd target)

        -- Send from close planets, first
        (localShips, localPlanets) = close $ groupBy ((==) `on` distTarget)
                                           $ sortBy (comparing distTarget)
                                           $ mpl
          where
            distTarget = distance (fst target)
            close = close' 0 []
            close' have chosen           [] = (have, chosen)
            close' have chosen avail@(x:xs)
              | totalFleetSize <= have = (have, chosen)
              | otherwise              = close' (have + got) (chosen ++ x) xs
              where
                got = sum $ map planetShips x

        -- Per-planet fleet size
        fleetSize p = (totalFleetSize * planetShips p) `divCeil` localShips
          where
            divCeil x y = if r > 0 then q + 1 else q
              where (q, r) = x `divMod` y

        -- Calculate the first set of orders
        delta p = (order, p')
          where
            fleet = fleetSize p
            order = orderViaWaypoints (planetId p) (planetId $ fst target) fleet
            p' = p { planetShips = planetShips p - fleet }
        (all_orders, mpl') = unzip $ map delta localPlanets
        orders = filter valid all_orders
          where
            srcNotEqualDest o = orderSource o /= orderDestination o
            positiveFleetSize o = orderShips o > 0
            valid o = positiveFleetSize o && srcNotEqualDest o

        mp' = (IM.fromList $ map ((,) <$> planetId <*> id) mpl') `IM.union` mp

main :: IO ()
main = bot doTurn
