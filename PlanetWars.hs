-- | Library for the Planet Wars google ai contest. More information can be
-- found on http://ai-contest.com.
--
{-# LANGUAGE FlexibleInstances #-}
module PlanetWars
    ( 
      -- * Data structures
      Resource (..)
    , Planet (..)
    , Fleet (..)
    , Order (..)
    , GameState (..)

      -- * Utility functions
    , isAllied
    , isHostile
    , isNeutral
    , addShips
    , fleetIsArrived
    , getPlanetById
    , distanceBetween
    , centroid
    , isArrived
    , planetById
    , willSurviveAttack
    , currentOwner
    , planetsUnderAttack
    , incomingFleets

    , stepAllFleets
      -- * Communication with the game engine
    , issueOrder
    , finishTurn

      -- * Bots
    , bot
    , ioBot
    , debugBot

      -- ^ Engine / Simulation
    , departureNoFailReport
    , simpleDeparture
    , advancement
    , arrival
    , engineTurn
    , engineTurnNoReport
    , engineTurnNoOrders
    , simpleEngineTurn

      -- * Debugging
    , stateFromFile
    , unique
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate, isPrefixOf, partition, foldl', sortBy)
import Data.Maybe (fromJust)
import Data.Monoid (Monoid, mempty, mappend)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Ord (comparing)
import System.IO
import System.IO.Error (isEOFError)

-- | Class for values that are owned by a player
--
class Resource a where
    owner :: a -> Int

-- | Class for physical entities
--
class Entity a where
    getX :: a -> Double
    getY :: a -> Double

instance Entity (Double, Double) where
    getX = fst
    getY = snd

-- | Representation of a planet
--
data Planet = Planet
    { planetId         :: Int
    , planetOwner      :: Int
    , planetShips      :: Int
    , planetGrowthRate :: Int
    , planetX          :: Double
    , planetY          :: Double
    } deriving (Show)

instance Resource Planet where
    owner = planetOwner

instance Entity Planet where
    getX = planetX
    getY = planetY

-- | Representation of a fleet
--
data Fleet = Fleet
    { fleetOwner          :: Int
    , fleetShips          :: Int
    , fleetSource         :: Int
    , fleetDestination    :: Int
    , fleetTripLength     :: Int
    , fleetTurnsRemaining :: Int
    } deriving (Show)

instance Resource Fleet where
    owner = fleetOwner

-- | Check that fleet is arrived
--
fleetIsArrived :: Fleet -> Bool
fleetIsArrived = (<=0) . fleetTurnsRemaining

-- | Representation of an order
--
data Order = Order
    { orderSource      :: Int
    , orderDestination :: Int
    , orderShips       :: Int
    } deriving (Show)

-- | A data structure describing the game state.
--
-- * Planets are mapped by id
--
-- * Fleets are mapped by destination
--
data GameState = GameState
    { gameStatePlanets :: IntMap Planet
    , gameStateFleets  :: [Fleet]
    } deriving (Show)

instance Monoid GameState where
    mempty = GameState mempty mempty
    mappend (GameState p1 f1) (GameState p2 f2) =
        GameState (p1 `mappend` p2) (f1 `mappend` f2)

-- | Find planet in GameState with given planetId
--
getPlanetById :: Int -> GameState -> Planet
getPlanetById pid state = (IM.!) (gameStatePlanets state) pid

-- | Auxiliary function for parsing the game state. This function takes an
-- initial state, and a line. The line is parsed and content is applied on the
-- given state. Folding with this function can produce the entire game state.
--
buildGameState :: GameState  -- ^ Initial game state
               -> String     -- ^ Line to parse and apply
               -> GameState  -- ^ Resulting game state
buildGameState state string = case words string of
    ("P" : xs) ->
        let planet = Planet planetId'
                            (read $ xs !! 2)
                            (read $ xs !! 3)
                            (read $ xs !! 4)
                            (read $ xs !! 0)
                            (read $ xs !! 1)
        in state { gameStatePlanets = IM.insert planetId' planet
                                                (gameStatePlanets state)
                 }
    ("F" : xs) ->
        let fleet = Fleet (read $ xs !! 0)
                          (read $ xs !! 1)
                          (read $ xs !! 2)
                          (read $ xs !! 3)
                          (read $ xs !! 4)
                          (read $ xs !! 5)
        in state { gameStateFleets = fleet : gameStateFleets state
                 }
    _ -> state
  where
    planetId' = IM.size $ gameStatePlanets state

-- | Check if a given resource is allied
--
isAllied :: Resource r => r -> Bool
isAllied = (== 1) . owner

-- | Check if a given resource is hostile
--
isHostile :: Resource r => r -> Bool
isHostile = (> 1) . owner

-- | Check if a given resource is neutral
--
isNeutral :: Resource r => r -> Bool
isNeutral = (<= 0) . owner

-- | Perform the 'Departure' phase of game state update
--
departure :: IntMap [Order] -- ^ Orders, grouped by issuing player
          -> GameState      -- ^ Old game state
          -> ( [Int]        -- ^ Players that gave an invalid order
             , GameState    -- ^ New game state
             )
departure ordersMap gs = ( droppedPlayers, gs' )
  where
    ( droppedPlayers, gs' ) = foldl' accum ([], gs) $ IM.assocs
                            $ IM.mapWithKey doOrders ordersMap
    accum (ps, lgs) (p, f) | valid     = (ps, lgs')
                           | otherwise = (p:ps, dropPlayer p lgs)
      where
        (valid, lgs') = f lgs

    -- Remove a player from the game.
    dropPlayer player lgs =
        lgs { gameStatePlanets = IM.map setNeutral $ gameStatePlanets lgs
            , gameStateFleets  = filter ((/= player) . fleetOwner)
                               $ gameStateFleets lgs
            }
      where
        setNeutral planet = if planetOwner planet == player
            then planet { planetOwner = 0 }
            else planet

    -- Spawn fleets based on orders from a single player
    doOrders _     [] lgs = ( True, lgs )
    doOrders p (o:os) lgs = ( headValid && tailValid, lgs'' )
      where
        ( headValid, lgs' ) = doOrder p o lgs
        ( tailValid, lgs'' ) = doOrders p os lgs'

    -- Spawn a fleet based on a single order from a single player
    doOrder p o lgs = ( valid, lgs' )
      where
        sourceId = orderSource o
        sourcePlanet = planetById lgs sourceId
        sourceShips = planetShips sourcePlanet
        movingShips = orderShips o
        destinationId = orderDestination o
        tripLength = ceiling
                   $ distanceBetween sourcePlanet (planetById lgs destinationId)
        valid = planetOwner sourcePlanet == p
              && sourceId /= destinationId
              && sourceShips >= movingShips
        lgs' = if valid
                  && movingShips > 0 -- Ignore size 0 fleets for now
            then lgs { gameStatePlanets =
                           IM.singleton sourceId sourcePlanet
                               { planetShips = sourceShips - movingShips
                               } `IM.union` gameStatePlanets lgs
                     , gameStateFleets = Fleet
                           { fleetOwner = p
                           , fleetShips = movingShips
                           , fleetSource = sourceId
                           , fleetDestination = destinationId
                           , fleetTripLength = tripLength
                           , fleetTurnsRemaining = tripLength
                           } : gameStateFleets lgs
                     }
            else lgs

-- | Perform the 'Departure' phase, but do not report what players are
-- | dropped.  Those players are still dropped, it just isn't reported.
--
departureNoFailReport :: IntMap [Order] -- ^ Orders, grouped by issuing player
                      -> GameState      -- ^ Old game state
                      -> GameState      -- ^ New game state
departureNoFailReport = (snd <$>) <$> departure

-- | Perform the 'Departure' phase of game state update with no orders
--
simpleDeparture :: GameState -- ^ Old game state
                -> GameState -- ^ New game state
simpleDeparture = departureNoFailReport IM.empty

-- | Perform the 'Advancement' phase of game state update
--
advancement :: GameState -- ^ Old game state
            -> GameState -- ^ New game state
advancement gs = gs { gameStatePlanets = IM.map advancePlanet
                                       $ gameStatePlanets gs
                    , gameStateFleets = map advanceFleet $ gameStateFleets gs
                    }
  where
    advancePlanet p | isNeutral p = p
                    | otherwise   = p { planetShips = planetShips p
                                                    + planetGrowthRate p }
    advanceFleet f = f { fleetTurnsRemaining = fleetTurnsRemaining f - 1 }

-- | Perform the 'Arrival' phase of game state update
--
arrival :: GameState -- ^ Old game state
        -> GameState -- ^ New game state
arrival gs = gs { gameStatePlanets =
                      IM.map (uncurry resolveCombat) planetsAndForces
                      `IM.union` gameStatePlanets gs
                , gameStateFleets  = remainingFleets
                }
  where
    -- Make a fleet representing the defence forces on a planet.
    planetsAndFleets = IM.map ((,) <$> id <*> (IM.singleton <$> planetOwner
                                                            <*> planetShips))
                     $ gameStatePlanets gs

    -- Pull out arriving fleets for processing leaving fleets still in
    -- transit for later.
    (arrivingFleets, remainingFleets) = partition ((== 0) . fleetTurnsRemaining)
                                      $ gameStateFleets gs

    -- Make forces that are the sum of fleets arriving
    fleetForces = IM.unionsWith (IM.unionWith (+))
                $ map fleetForce arrivingFleets
    fleetForce = IM.singleton <$> fleetDestination
               <*> (IM.singleton <$> fleetOwner <*> fleetShips)

    -- Combine the planet fleets with the other fleets at that planet
    planetsAndForces = IM.map (IM.assocs <$>)
                     $ IM.intersectionWith combine planetsAndFleets fleetForces
      where
        combine (planet, fleet) forces = ( planet
                                         , IM.unionWith (+) fleet forces
                                         )

    -- Resolve a combat at a planet with 0 or more forces
    resolveCombat p       [] = p -- This should not happen.
    resolveCombat p [(o, s)] = p { planetOwner = o, planetShips = s }
    resolveCombat p   forces | ships > 0 = p { planetOwner = fst bigWinner
                                             , planetShips = ships
                                             }
                             | otherwise = p { planetShips = ships }
      where
        (bigWinner:bigLoser:_) = sortBy (flip $ comparing snd) forces
        ships = snd bigWinner - snd bigLoser

-- | Do a full game state update based on the orders received
--
engineTurn :: IntMap [Order] -- ^ Orders grouped by issuing player
           -> GameState      -- ^ Old game state
           -> ( ( Bool       -- ^ Game over?
                , Maybe Int  -- ^ Winner, if there is one
                , [Int]      -- ^ Players that lost this turn
                )
              , GameState    -- ^ New game state
              )
engineTurn ordersMap gs = ( ( gameOver, winner , dropped ++ losers), gs' )
  where
    -- Departure phase
    (dropped, gs'') = departure ordersMap gs

    -- Who is left after that?
    planetPlayers' = IM.elems . IM.map planetOwner $ gameStatePlanets gs''
    fleetPlayers' = map fleetOwner $ gameStateFleets gs''
    (_, notDroppedPlayers) = IS.split 0 . IS.fromList
                           $ planetPlayers' ++ fleetPlayers'

    -- Advancement and arrival phases
    gs' = arrival $ advancement gs''

    -- Who is left after that?
    planetPlayers = IM.elems . IM.map planetOwner $ gameStatePlanets gs'
    fleetPlayers = map fleetOwner $ gameStateFleets gs'
    (_, remainingPlayers) = IS.split 0 . IS.fromList $ planetPlayers ++ fleetPlayers

    -- Find the losers
    losers = IS.elems $ remainingPlayers IS.\\ notDroppedPlayers

    -- Find the winner and end the game
    countRemaining = IS.size remainingPlayers
    gameOver = countRemaining < 2
    winner | countRemaining == 1 = Just . head $ IS.elems remainingPlayers
           | otherwise           = Nothing

-- | Do a full game state update, but don't report game over, winner, or losers
--
engineTurnNoReport :: IntMap [Order] -- ^ Orders groups by issuing players
                   -> GameState      -- ^ Old game state
                   -> GameState      -- ^ New game state
engineTurnNoReport = ((arrival . advancement) .) . departureNoFailReport

-- | Do a full game state update as if no players gave any orders
--
engineTurnNoOrders :: GameState     -- ^ Old game state
                   -> ( ( Bool      -- ^ Game Over?
                        , Maybe Int -- ^ Winner, if there is one.
                        , [Int]     -- ^ Players that lost this turn
                        )
                      , GameState   -- ^ New game state
                      )
engineTurnNoOrders = engineTurn IM.empty

-- | Do a full game state update as if no players gave any orders but don't
-- | report game over, winner, or losers
--
simpleEngineTurn :: GameState -- ^ Old game state
                 -> GameState -- ^ New game state
simpleEngineTurn = engineTurnNoReport IM.empty

-- | Add (or subtract) a number of ships to (or from) a planet
--
addShips :: Planet  -- ^ Planet to add ships to
         -> Int     -- ^ Number of ships to add
         -> Planet  -- ^ Resulting planet
addShips planet n = planet {planetShips = planetShips planet + n}

-- | Find the distance between two planets
--
distanceBetween :: (Entity a, Entity b) => a -> b -> Double
distanceBetween p1 p2 = let dx = getX p1 - getX p2
                            dy = getY p1 - getY p2
                        in sqrt $ dx * dx + dy * dy

-- | Find the centroid of the given planets
--
centroid :: IntMap Planet -> (Double, Double)
centroid planets = div' $ IM.fold add' (0, 0) planets
  where
    add' planet (x, y) = (x + planetX planet, y + planetY planet)
    div' (x, y) = let size = fromIntegral $ IM.size planets
                  in (x / size, y / size)

-- | Check if a fleet has arrived
--
isArrived :: Fleet -> Bool
isArrived = (== 0) . fleetTurnsRemaining

-- | Get a planet by ID. Make sure the ID exists!
--
planetById :: GameState -> Int -> Planet
planetById state id' = fromJust $ IM.lookup id' $ gameStatePlanets state

stepAllFleets :: GameState -> GameState
stepAllFleets state | null (gameStateFleets state) = state
                    | otherwise = stepAllFleets $ simpleEngineTurn state

-- | Issue an order
--
issueOrder :: Order  -- ^ Order to execute
           -> IO ()  -- ^ Result
issueOrder (Order source destination ships) =
    putStrLn $ intercalate " " $ map show [source, destination, ships]

-- | Finish your turn
--
finishTurn :: IO ()   -- ^ Result
finishTurn = do
    putStrLn "go"
    hFlush stdout

-- | Run a deterministic bot
--
bot :: (GameState -> [Order])  -- ^ Deterministic AI function
    -> IO ()                   -- ^ Blocks forever
bot f = ioBot $ mapM_ issueOrder . f

-- | Run an IO bot. This is a more liberal version of 'bot', which allows you to
-- work in the IO monad. However, you need to call 'issueOrder' yourself if you
-- use this function -- 'finishTurn' will still be called automatically.
--
ioBot :: (GameState -> IO ())  -- ^ Bot action
      -> IO ()                 -- ^ Blocks forever
ioBot f = do
    hSetBuffering stdin NoBuffering
    catch (loop mempty) $ \e -> if isEOFError e
        then return ()
        else ioError e
  where
    loop state = do
        line <- takeWhile (/= '#') <$> getLine
        if "go" `isPrefixOf` line
            -- Go Go Go!
            then do
                f state
                finishTurn
                loop mempty
            -- Keep building map
            else loop (buildGameState state line)

-- | Run a deterministic bot, dumping debug info
--
debugBot :: (GameState -> [Order])  -- ^ Deterministic AI function(
         -> IO ()                   -- ^ Blocks forever
debugBot f = do
    h <- openFile "debug.log" WriteMode 
    ioBot $ (run h)
    hClose h
  where
    run h s = do
        orders <- stateDump h s
        dumpIssue h orders
    dumpIssue hlog orders = do
        hPutStrLn hlog "\nOrders:"
        hPutStrLn hlog $ show orders
        hFlush hlog
        mapM_ issueOrder orders
    stateDump hlog s = do
        hPutStrLn hlog "\nState:"
        hPutStrLn hlog $ show s
        hFlush hlog
        return $ f s

-- | Read a game state from file. The format is the same as the server's output
-- for a turn. Useful when debugging.
--
stateFromFile :: FilePath     -- ^ Path to the file containing the game state.
              -> IO GameState -- ^ Parsed game state
stateFromFile path = withFile path ReadMode (loop mempty)
  where
    loop state handle = do
      line <- takeWhile (/= '#') <$> hGetLine handle
      if "go" `isPrefixOf` line
        then return state
        else loop (buildGameState state line) handle

-- | Checks if a planet will survive the incoming fleets. A planet survives if
-- its owner is still the same after all known fleets arrive.
--
willSurviveAttack :: GameState -- ^ Initial game state
                  -> Int       -- ^ Planet ID
                  -> Bool      -- ^ Whether the planet survived
willSurviveAttack state pid = survives state
  where
    originalOwner = currentOwner state pid
    survives s = if null $ incomingFleets s pid
      then currentOwner s pid == originalOwner
      else survives $ simpleEngineTurn s

-- | The owner of a planet in a given game state.
--
currentOwner :: GameState -- ^ Current game state
             -> IM.Key    -- ^ Planet ID
             -> Int       -- ^ Owner ID
currentOwner state pid = owner $ gameStatePlanets state IM.! pid

-- | List of planets under attack, i.e., that have incoming fleets.
--
planetsUnderAttack :: GameState -- ^ Game state to analyze
                   -> [Int]     -- ^ List of IDs of planets under attack
planetsUnderAttack = (map fleetDestination) . gameStateFleets

-- | List of incoming fleets for a given planet in a certain game state.
--
incomingFleets :: GameState -- ^ Game state containing the current fleets
               -> Int       -- ^ Planet ID
               -> [Fleet]   -- ^ Incoming fleets
incomingFleets state pid = filter pidMatches fleets
  where
    pidMatches = (== pid) . fleetDestination
    fleets = gameStateFleets state

-- | Removes duplicates from a list of Ints
--
unique :: [Int] -> [Int]
unique = IS.toList . IS.fromList

