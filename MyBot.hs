-- | Example bot using the PlanetWars module
--
module Main where

import Control.Applicative ((<$>), (<*>))
-- import Control.Monad (forM_, unless)

import Data.Function (on)
import Data.List (partition, sortBy, groupBy, maximumBy)
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Ord (comparing)
import System.Timeout (timeout)

import PlanetWars

-- | Divide, rounding up, without using floating point
--
infixl 7 `divCeil`
divCeil :: Integral a => a -> a -> a
divCeil x y = if r > 0 then q + 1 else q
  where (q, r) = x `divMod` y

-- | Distribute items among bins
--
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

extendOn :: (a -> b) -> a -> (a, b)
extendOn = ((,) . id <*>)

reduceOrders :: [Order] -> [Order]
reduceOrders xs = concat $ IM.elems $ IM.map IM.elems orderMap
  where
    orderMap = IM.mapWithKey (IM.mapWithKey . Order) shipsMap
    shipsMap = IM.unionsWith (IM.unionWith (+)) $ map shipMap xs
    shipMap = IM.singleton . orderSource
            <*> (IM.singleton . orderDestination <*> orderShips)

-- | Predict future game states assuming no orders from either side
-- | Since our simulation engine does know about the turn limit, this
-- | is likely an infinite list
--
predict :: GameState   -- ^ Game State
        -> [GameState] -- ^ Future game states
predict s = s : if over then [s'] else predict s'
  where ((over, _, _), s') = engineTurnNoOrders s

-- | Take predictions an key then based on time.  Limit the results
-- | to certain number of turns, since predictions might infinite and
-- | maps are strict on their keys
--
futureByTime :: Int              -- ^ Maximum time
             -> GameState        -- ^ Initial Game State
             -> IntMap GameState -- ^ Future game states indexed by time
futureByTime n = IM.fromList . zip [0..n] . predict

-- | Find the last turn predicted, which is the maximum key in the map.
-- | This function will fail miserably if the map is empty.
--
endOfTime :: IntMap GameState -- ^ Future same states indexed by time
          -> Int              -- ^ Maximum time index
endOfTime = fst . fst . maybe (error "Main.endOfTime") id . IM.maxViewWithKey

alliedShips :: Planet -- ^ Any planet
            -> Int    -- ^ Ships owned by me on that planet.
alliedShips p = if isAllied p then planetShips p else 0

shipsAvailableAlways :: IntMap GameState -- ^ Predicted game states
                     -> IntMap Int      -- ^ Ships available throughout the prediction
shipsAvailableAlways = IM.filter (/= 0) . IM.unionsWith min . IM.elems
                     . IM.map (IM.map alliedShips . gameStatePlanets)

refinementTimeout :: Int
refinementTimeout = 900000

refineTurn :: (GameState -> IO [Order]) -> GameState -> IO ()
refineTurn strategy gs = do
    bin <- newIORef []
    let
        loop state = do
            orders <- strategy state
            orders `seq` writeIORef bin orders
            opp_orders <- strategy . projectGameState 2
                                   . departureNoFailReport (IM.singleton 1 orders)
                                   $ gs
            loop $ departureNoFailReport (IM.singleton 2 opp_orders) gs
    _ <- timeout refinementTimeout $ loop gs
    orders <- readIORef bin
    mapM_ issueOrder orders

doTurn :: GameState  -- ^ Game state
       -> [Order]    -- ^ Orders
doTurn state = if IM.null myPlanets
    -- No valid orders
    then []
    else let
        (attacks, state') = attackOrders state
        (redeployments, state'') = redeployOrders state'
        fleeing = fleeOrders state''
    in reduceOrders $ attacks ++ redeployments ++ fleeing
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

    fleeOrders :: GameState -- ^ Current game state
               -> [Order]   -- ^ Proposed flights
    fleeOrders gs | myShips > theirShips = []
                  | otherwise            = flee =<< IM.elems lostPlanets
      where
        stateByTime = futureByTime maxTransitTime gs
        scheduleLimit = endOfTime stateByTime

        next = stateByTime IM.! 1
        theirPlanetsSoon = IM.filter isHostile $ gameStatePlanets next

        lostPlanets = IM.intersection myPlanets theirPlanetsSoon

        flee p | null possibleDestinations = []
               | otherwise                 =
              zipWith (Order sourceId) (map planetId destinations)
            $ filter (/= 0) $ planetShips p `pidgeonhole` length destinations
          where
            sourceId = planetId p
            planetsWithDistance = map (extendOn $ distanceById sourceId . planetId)
                                $ IM.elems $ IM.delete sourceId planetsMap
            validDestination q d = d <= scheduleLimit && isAllied futurePlanet
              where futurePlanet = planetById (stateByTime IM.! d) $ planetId q
            possibleDestinations = filter (uncurry validDestination)
                                 $ planetsWithDistance
            destinations = sortBy (comparing planetShips) $ map fst $ head
                         $ groupBy ((==) `on` snd)
                         $ sortBy (comparing snd) possibleDestinations

    attackOrders :: GameState    -- ^ Old game state
                 -> ( [Order]    -- ^ Proposed deployments
                    , GameState  -- ^ New game state
                    )
    attackOrders gs
      | IM.null availableShips = ([], gs)
      | null targets           = ([], gs)
      | otherwise              = let
        (more, final) = attackOrders next
      in (now_orders ++ more, final)
      where
        -- Predict the future given current game state
        stateByTime = futureByTime maxTransitTime gs
        scheduleLimit = endOfTime stateByTime
        availableShips = shipsAvailableAlways stateByTime

        -- Extend the planet structure with some useful data
        extendPlanet t p = (p, (t, shipsToConquer, shipsLost))
          where
            shipsPresent = planetShips p
            -- Calculate cost for the planet
            (shipsToConquer, shipsLost) = if isAllied p
                then (0, 0)
                else if t > 0 && (isAllied $ planetById (stateByTime IM.! (t - 1)) $ planetId p)
                then (shipsPresent, shipsPresent)
                else (shipsPresent + 1, shipsPresent)
        extendGameState t = IM.elems . IM.map ep . gameStatePlanets
          where
            ep = extendPlanet t

        -- Heuristic value of a planet.
        value (p, (t, s, l)) = (reward, t, s, l)
          where
            -- Calculate reward for the planet
            reward = if isHostile p then -growth else l - growth
            growth = planetGrowthRate p * growthFactor * growthTime
            growthFactor = if isNeutral p
                then 1
                else 2
            growthTime = max 0 $ scheduleLimit - t

        -- Target the "best" planet that we have enough ships to take.
        targets = map fst $ sortBy (comparing snd)
                $ filter suitable $ map (extendOn value) $ concat $ IM.elems
                $ IM.mapWithKey extendGameState stateByTime
        suitable ((p, (t, s, _)), v) = reward <= 0 && 0 < s && s <= shipsInRange
          where
            (reward, _, _, _) = v
            shipsInRange = sum $ IM.elems
                         $ IM.filterWithKey inRange
                         $ availableShips
            inRange pid = const $ distanceById (planetId p) pid <= t
        (target:_) = targets

        -- Send at least enough ships to conquer it.
        (targetPlanet, (targetTime, targetFleetSize, _)) = target
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
        now_orders = filter ((== targetTime) . distanceById (planetId targetPlanet) . orderSource) orders

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
main = ioBot . refineTurn $ return . doTurn
