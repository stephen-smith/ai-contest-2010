-- | Example bot using the PlanetWars module
--
module Main where

import Control.Applicative ((<$>), (<*>))
-- import Control.Monad (forM_, unless)

import Data.Function (on)
import Data.List (partition, sortBy, groupBy, maximumBy)
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Data.Ord (comparing)

-- import qualified System.IO.Unsafe (unsafePerformIO)

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

predict :: GameState   -- ^ Game State
        -> [GameState] -- ^ Future game states
predict s = s : if over then [s'] else predict s'
  where ((over, _, _), s') = engineTurnNoOrders s

futureByTime :: Int              -- ^ Maximum time
             -> GameState        -- ^ Initial Game State
             -> IntMap GameState -- ^ Future game states indexed by time
futureByTime n = (IM.fromList . zip [0..n]) <$> predict

endOfTime :: IntMap GameState -- ^ Future same states indexed by time
          -> Int              -- ^ Maximum time index
endOfTime = fst . fst . maybe (error "Main.endOfTime") id . IM.minViewWithKey

alliedShips :: Planet -- ^ Any planet
            -> Int    -- ^ Ships owned by me on that planet.
alliedShips p = if isAllied p then planetShips p else 0

shipsAvailableAlways :: IntMap GameState -- ^ Predicted game states
                     -> IntMap Int      -- ^ Ships available throughout the prediction
shipsAvailableAlways = IM.filter (/= 0) . IM.unionsWith min . IM.elems
                     . IM.map (IM.map alliedShips . gameStatePlanets)

doTurn :: GameState  -- ^ Game state
       -> [Order]    -- ^ Orders
doTurn state = if IM.null myPlanets
    -- No valid orders
    then []
    else let
        (attacks, state') = attackOrders state
        (redeployments, state'') = redeployOrders state'
        fleeing = fleeOrders state''
    in{- System.IO.Unsafe.unsafePerformIO $ do
        return $ -}reduceOrders $ attacks ++ redeployments ++ fleeing
  where
    (myFleets, enemyFleets) = partition isAllied $ gameStateFleets state

    planetsMap = gameStatePlanets state

    -- Cache distance calculations.
    distances :: IM.IntMap (IM.IntMap Int)
    distances = IM.map oneToMany planetsMap
      where
        oneToMany p = IM.map (turnsBetween p) planetsMap
        turnsBetween p q = ceiling $ distanceBetween p q

    distanceById x y = (distances IM.! x) IM.! y

    planetIds = IM.keys planetsMap
    maxTransitTime = maximum (distanceById <$> planetIds <*> planetIds)

    -- Partition all planets
    (myPlanets, notMyPlanets) = IM.partition isAllied planetsMap
    enemyPlanets = IM.filter isHostile notMyPlanets

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

    -- Calculate way-points
    waypoints = IM.mapWithKey oneToMany planetsMap
      where
        oneToMany pid = const $ IM.mapWithKey (allWaypoints pid) planetsMap
        allWaypoints pid qid = const $ IM.elems
                             $ IM.filterWithKey isWaypoint planetsMap
          where
            dz = distanceById pid qid
            isWaypoint rid = const $ pid /= rid && rid /= qid && dx + dy <= dz
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

    -- Heuristic value of a planet.
    value (p, (t, s)) = (s - growth, t, s)
      where
        -- Calculate reward for the planet
        growth = planetGrowthRate p * growthFactor * growthTime
        growthFactor = if isNeutral p
            then 1
            else 2
        growthTime = max 0 $ maxTransitTime - t

    fleeOrders :: GameState -- ^ Current game state
               -> [Order]   -- ^ Proposed flights
    fleeOrders gs | myShips > theirShips = []
                  | IM.null keptPlanets  = []
                  | otherwise            = flee =<< IM.elems lostPlanets
      where
        stateByTime = futureByTime maxTransitTime gs
        scheduleLimit = endOfTime stateByTime

        next = stateByTime IM.! 1
        theirPlanetsSoon = IM.filter isHostile $ gameStatePlanets next

        lost = const . (`IM.member` theirPlanetsSoon)
        (lostPlanets, keptPlanets) = IM.partitionWithKey lost myPlanets

        flee p = zipWith (Order sourceId) (map planetId destinations)
               $ filter (/= 0) $ planetShips p `pidgeonhole` length destinations
          where
            sourceId = planetId p
            planetsWithDistance = map ((,) <$> id
                                           <*> (distanceById sourceId . planetId))
                                $ IM.elems keptPlanets
            validDestination q d = d <= scheduleLimit && isAllied futurePlanet
              where futurePlanet = planetById (stateByTime IM.! d) $ planetId q
            destinations = sortBy (comparing planetShips) $ map fst $ head
                         $ groupBy ((==) `on` snd)
                         $ sortBy (comparing snd)
                         $ filter (uncurry validDestination) planetsWithDistance

    attackOrders :: GameState    -- ^ Old game state
                 -> ( [Order]    -- ^ Proposed deployments
                    , GameState  -- ^ New game state
                    )
    attackOrders gs
      | IM.null availableShips = ([], gs)
      | null targets  ={- System.IO.Unsafe.unsafePerformIO $ do
        forM_ hardTargets $ \(p, (t, s)) -> do
            putStrLn $ show (planetId p, t, s)
        return $ -}([], gs)
      | otherwise         = let
        (more, final) = attackOrders next
      in{- System.IO.Unsafe.unsafePerformIO $ do 
        forM_ hardTargets $ \(p, (t, s)) -> do
            putStrLn $ show (planetId p, t, s)
        putStrLn $ show (planetId $ fst target, snd target)
        forM_ all_orders $ \o -> do
            putStrLn $ show o
        return $ -}(now_orders ++ more, final)
      where
        -- Predict the future given current game state
        stateByTime = futureByTime maxTransitTime gs
        availableShips = shipsAvailableAlways stateByTime

        -- Extend the planet structure with some useful data
        extendPlanet t p = (p, (t, shipsToConquer))
          where
            -- Calculate cost for the planet
            shipsToConquer = if isAllied p
                then 0
                else if t > 0 && (isAllied $ planetById (stateByTime IM.! (t - 1)) $ planetId p)
                then planetShips p
                else planetShips p + 1
        extendGameState t = IM.elems . IM.map ep . gameStatePlanets
          where
            ep = extendPlanet t

        -- Target the "best" planet that we have enough ships to take.
        targets = sortBy (comparing value) $ filter (not . tooHard)
                $ filter ((> 0) . snd .snd) $ concat
                $ IM.elems $ IM.mapWithKey extendGameState stateByTime
        tooHard (p, (t, s)) = s > shipsInRange
          where
            shipsInRange = sum $ IM.elems
                         $ IM.filterWithKey inRange
                         $ availableShips
            inRange pid = const $ distanceById (planetId p) pid <= t
        (target:_) = targets

        -- Send at least enough ships to conquer it.
        targetFleetSize = (snd $ snd target)
        targetPlanet = fst target
        distTarget = distanceById $ planetId targetPlanet

        -- Send from close planets, first
        (localShips, localPids) = close $ groupBy ((==) `on` dist)
                                $ sortBy (comparing dist)
                                $ IM.assocs availableShips
          where
            dist = distTarget . fst
            close = close' 0 []
            close' have chosen     [] = (have, chosen)
            close' have chosen (x:xs)
              | targetFleetSize <= have = (have, chosen)
              | otherwise               = close' (have + got) (chosen ++ from) xs
              where
                got = sum $ map snd x
                from = map fst x

        -- Per-planet fleet size
        fleetSize = (`divCeil` localShips) . (* targetFleetSize)
                  . ((IM.!) availableShips)

        -- Calculate the first set of orders
        order pid = orderViaWaypoints pid (planetId targetPlanet) (fleetSize pid)
        positiveFleetSize = (> 0) . orderShips
        all_orders = filter positiveFleetSize $ map order localPids
        srcNotEqualDest = (/=) <$> orderSource <*> orderDestination
        (orders, _) = partition srcNotEqualDest all_orders
        now_orders = filter ((== (fst $ snd target)) . distanceById (planetId targetPlanet) . orderSource) orders

        -- Partially advance game gs for future calculations
        next = departureNoFailReport (IM.singleton 1 orders) gs

    redeployOrders :: GameState
                   -> ( [Order]
                      , GameState
                      )
    redeployOrders gs | IM.null myDistancesToAttackable = ([], gs)
                      | otherwise                       = (orders, gs')
      where
        stateByTime = futureByTime maxTransitTime gs
        scheduleLimit = endOfTime stateByTime
        availableShips = shipsAvailableAlways stateByTime

        myDistances = distances `IM.intersection` myPlanets
        myDistancesToAttackable = IM.filter (not . IM.null)
                                $ IM.map (`IM.intersection` notMyPlanets)
                                $ myDistances
        myDistancesToEnemy = IM.filter (not . IM.null)
                           $ IM.map (`IM.intersection` enemyPlanets)
                           $ myDistancesToAttackable

        minAttackDistances = IM.map (minimum . IM.elems)
                           $ myDistancesToAttackable
        minEnemyDistances = IM.map (minimum . IM.elems)
                          $ myDistancesToEnemy

        redeployPid pid ships
          | IM.null closer              = []
          | transitTime > scheduleLimit = []
          | IM.null myDistancesToEnemy  = planetOrders
          | 2 * transitTime > enemyDist = []
          | otherwise                   = planetOrders
          where
            -- This planet's distance from an attackable planet
            attackDist = minAttackDistances IM.! pid

            -- Allied planets that are closer
            closer = IM.filter (< attackDist) $ IM.delete pid minAttackDistances

            -- Find the closest enemy, which limits travel time
            enemyDist = minEnemyDistances IM.! pid

            -- Determine where to send
            closest = head $ groupBy ((==) `on` snd) $ sortBy (comparing snd)
                    $ IM.assocs
                    $ IM.mapWithKey (const . distanceById pid) closer
            transitTime = snd $ head closest
            destinations = map fst closest
            sizes = filter (/= 0) $ ships `pidgeonhole` length destinations

            -- Make orders
            planetOrders = zipWith (orderViaWaypoints pid) destinations sizes

        orders = concat $ IM.elems
               $ IM.mapWithKey redeployPid availableShips

        gs' = departureNoFailReport (IM.singleton 1 orders) gs

main :: IO ()
main = bot doTurn
