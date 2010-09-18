-- | Example bot using the PlanetWars module
--
module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_)

import Data.Function (on)
import Data.List (partition, sortBy, groupBy, maximumBy)
import qualified Data.IntMap as IM
import Data.Ratio ((%), numerator, denominator)
import Data.Ord (comparing)

import qualified System.IO.Unsafe (unsafePerformIO)

import PlanetWars

infixl 7 `divCeil`
divCeil :: Integral a => a -> a -> a
divCeil x y = if r > 0 then q + 1 else q
  where (q, r) = x `divMod` y

infixl 7 `pidgeonhole`
pidgeonhole :: Integral a => a -> a -> [a]
pidgeonhole x y | y < 0     = pidgeonhole x $ negate y
                | otherwise = replicate (fromIntegral r) (q + 1)
                            ++ replicate (fromIntegral (y - r)) q
  where (q, r) = x `divMod` y

splitWhile :: (a -> Bool) -> [a] -> ([a],[a])
splitWhile _ [] = ([], [])
splitWhile f l@(x:xs)
  | f x       = let (p, t) = splitWhile f xs in (x:p, t)
  | otherwise = ([], l)

reduceOrders :: [Order] -> [Order]
reduceOrders xs = concat $ IM.elems $ IM.map IM.elems orderMap
  where
    orderMap = IM.mapWithKey (IM.mapWithKey . Order) shipsMap
    shipsMap = IM.unionsWith (IM.unionWith (+)) $ map shipMap xs
    shipMap o = IM.singleton (orderSource o)
               $ IM.singleton (orderDestination o) (orderShips o)

doTurn :: GameState  -- ^ Game state
       -> [Order]    -- ^ Orders
doTurn state = if IM.null myPlanets
    -- No valid orders
    then []
    -- If we have a fleet in flight, just do nothing
    else let
        (attacks, state') = attackOrders myPlanetShips attackersAvailable state
        fleeing = fleeOrders state'
    in reduceOrders $ attacks ++ fleeing
  where
    (myFleets, enemyFleets) = partition isAllied $ gameStateFleets state
    (supportFleets, attackFleets) = partition isSupportFleet myFleets
      where
        isSupportFleet = isAllied . planetById state . fleetDestination

    planetsMap = gameStatePlanets state

    -- Cache distance calculations.
    distances :: IM.IntMap (IM.IntMap Int)
    distances = IM.map oneToMany planetsMap
      where
        oneToMany p = IM.map (turnsBetween p) planetsMap
        turnsBetween p q = ceiling $ distanceBetween p q

    distanceById x y = (distances IM.! x) IM.! y
    distance p q = distanceById (planetId p) (planetId q)

    planetIds = IM.keys planetsMap
    maxTransitTime = maximum (distanceById <$> planetIds <*> planetIds)

    -- Partition all planets
    myPlanets = IM.filter isAllied planetsMap

    -- Count ship totals
    (myPlanetShips, theirPlanetShips) = IM.fold accum (0, 0) planetsMap
      where
        accum p (n, m) | isHostile p = (n, m + planetShips p)
                       | isAllied  p = (n + planetShips p, m)
                       | otherwise   = (n, m)
    (myShips, theirShips) =
        (myPlanetShips + myFleetShips, theirPlanetShips + theirFleetShips)
      where
        [myFleetShips, theirFleetShips] =
            map (sum . map fleetShips) [myFleets, enemyFleets]

    -- Count production totals
    (myProduction, theirProduction) = IM.fold accum (0, 0) planetsMap
      where
        accum p (n, m) | isHostile p = (n, m + planetGrowthRate p)
                       | isAllied  p = (n + planetGrowthRate p, m)
                       | otherwise   = (n, m)

    -- Adjust the number of ships on aggressive manuvers
    need :: Rational -- Ranges over [0, 1]
    need = fromIntegral theirProduction
         % fromIntegral (theirProduction + myProduction)
    greed :: Rational -- Ranges over [0, 1]
    greed = fromIntegral myShips % fromIntegral (theirShips + myShips)
    aggressiveness = sqrt . rationalToDouble $ need * greed -- Ranges over [0, 1]
      where
        rationalToDouble :: Rational -> Double
        rationalToDouble = (/) <$> (fromIntegral . numerator)
                               <*> (fromIntegral . denominator)

    -- Determine the most ships to send attacking
    currentAttackers = sum $ map fleetShips attackFleets
    totalAttackers = ceiling $ fromIntegral myPlanetShips * aggressiveness
    attackersAvailable = totalAttackers - currentAttackers

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
        wps = filter isAllied $ (waypoints IM.! s) IM.! d
        wp = maximumBy (comparing $ distanceById s) $ map planetId wps

    -- Extend the planet structure with some useful data
    extendPlanet t p = (p, (t, shipsToConquer))
      where
        -- Calculate cost for the planet
        shipsToConquer = if isAllied p
            then 0
            else planetShips p + 1
    extendGameState t = IM.elems . IM.map ep . gameStatePlanets
      where
        ep = extendPlanet t

    -- Heuristic value of a planet.
    value (p, (t, s)) = (s - growth, t, s)
      where
        -- Calculate reward for the planet
        growth = planetGrowthRate p * growthFactor * growthTime
        growthFactor = if isNeutral p
            then 1
            else 2
        growthTime = if maxTransitTime < t
            then 0
            else maxTransitTime - t

    fleeOrders gs | myShips > theirShips = []
                  | IM.null keptPlanets  = []
                  | otherwise            = orders
      where
        next = simpleEngineTurn gs
        myPlanetsNow = IM.filter isAllied $ gameStatePlanets gs
        theirPlanetsSoon = IM.filter isHostile $ gameStatePlanets next
        lostPlanets = IM.intersection myPlanetsNow theirPlanetsSoon
        keptPlanets = IM.difference myPlanetsNow theirPlanetsSoon
        orders = concat $ map planetOrders $ IM.elems lostPlanets
        planetOrders p = zipWith (Order sourceId) (map planetId destinations)
                       $ planetShips p `pidgeonhole` length destinations
          where
            sourceId = planetId p
            planetsWithDistance = map ((,) <$> id
                                           <*> (distanceById sourceId . planetId))
                                $ IM.elems $ IM.delete sourceId keptPlanets
            destinations = sortBy (comparing planetShips) $ map fst $ head
                         $ groupBy ((==) `on` snd)
                         $ sortBy (comparing snd) planetsWithDistance

    attackOrders :: Int -> Int -> GameState -> ([Order], GameState)
    attackOrders       0      _ gs = ([], gs)
    attackOrders defence attack gs
      | null easyTargets  ={- System.IO.Unsafe.unsafePerformIO $ do
        putStrLn $ show defence
        putStrLn $ show attack
        forM_ (map ((,) <$> (planetId . fst) <*> snd) targets) $ \t -> do
            putStrLn $ show t
        return $ -}([], gs)
      | otherwise         = let
        (more, final) = attackOrders defence' attack' next
      in{- System.IO.Unsafe.unsafePerformIO $ do 
        putStrLn $ show defence
        putStrLn $ show attack
        putStrLn $ show (planetId $ fst target, snd target)
        putStrLn $ show (targetFleetSize, totalFleetSize)
        forM_ all_orders $ \o -> do
            putStrLn $ show o
        return $ -}(orders ++ more, final)
      where
        mpl = filter isAllied $ IM.elems $ gameStatePlanets gs

        -- Predict the future given current gs
        future = iterate simpleEngineTurn gs
        stateByTime = IM.fromList $ zip [0..maxTransitTime] future

        -- Target the "best" planet that we have enough ships to take.
        targets = sortBy (comparing value) $ filter ((> 0) . snd .snd) $ concat
                $ IM.elems $ IM.mapWithKey extendGameState stateByTime
        tooHard (p, (t, s)) = (if isAllied $ planetById gs $ planetId p
            then s > defence
            else s > attack) || s > shipsInRange
          where
            shipsInRange = sum $ map planetShips $ filter ((<= t) . distance p) mpl
        (_, easyTargets) = splitWhile tooHard targets
        (target:_) = easyTargets

        -- Send at least enough ships to conquer it.
        targetFleetSize = (snd $ snd target)

        -- Send from close planets, first
        (localShips, localPlanets) = close $ groupBy ((==) `on` distTarget)
                                           $ sortBy (comparing distTarget)
                                           $ mpl
          where
            distTarget = distance (fst target)
            close = close' 0 []
            close' have chosen     [] = (have, chosen)
            close' have chosen (x:xs)
              | targetFleetSize <= have = (have, chosen)
              | otherwise              = close' (have + got) (chosen ++ x) xs
              where
                got = sum $ map planetShips x

        -- Per-planet fleet size
        fleetSize p = (targetFleetSize * planetShips p) `divCeil` localShips
        totalFleetSize = sum $ map fleetSize localPlanets

        -- Remaining forces
        defence' = max 0 $ defence - totalFleetSize
        attack' = max 0 $ if isAllied $ planetById gs $ planetId $ fst target
            then attack
            else attack - totalFleetSize

        -- Calculate the first set of orders
        order p = orderViaWaypoints (planetId p) (planetId $ fst target) (fleetSize p)
        positiveFleetSize = (> 0) . orderShips
        all_orders = filter positiveFleetSize $ map order localPlanets
        srcNotEqualDest = (/=) <$> orderSource <*> orderDestination
        (orders, reserve) = partition srcNotEqualDest all_orders

        -- Modify game state to account for reserved forces
        planetReserve o = (s, p { planetShips = planetShips p - orderShips o })
          where
            s = orderSource o
            p = planetById gs s
        gs' = gs { gameStatePlanets = (IM.fromList $ map planetReserve reserve)
                                    `IM.union` gameStatePlanets gs }

        -- Partially advance game gs for future calculations
        next = departureNoFailReport (IM.singleton 1 orders) gs'

main :: IO ()
main = bot doTurn
