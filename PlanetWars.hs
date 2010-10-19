-- | Library for the Planet Wars google ai contest. More information can be
-- found on http://ai-contest.com.
--
module PlanetWars
    ( 
      -- * Data structures
      Resource (..)
    , Planet (..)
    , Fleet (..)
    , Order (..)
    , GameState (..)
    , makeGSI

      -- * Utility functions
    , isAllied
    , isHostile
    , isNeutral
    , addShips
    , fleetIsArrived
    , getPlanetById
    , distanceBetween
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
    ) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.Foldable as F
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

-- | Representation of a planet
--
data Planet = Planet
    { planetId         :: Int
    , planetOwner      :: Int
    , planetShips      :: Int
    , planetGrowthRate :: Int
    , planetX          :: Double
    , planetY          :: Double
    } deriving (Eq, Show)

instance Resource Planet where
    owner = planetOwner

data PlanetInput = PlanetInput
    { piOwner      :: Int
    , piShips      :: Int
    , piGrowthRate :: Int
    , piX          :: Double
    , piY          :: Double
    }

makePlanet pid input = Planet
    { planetId = pid
    , planetOwner = piOwner input
    , planetShips = piShips input
    , planetGrowthRate = piGrowthRate input
    , planetX = piX input
    , planetY = piY input
    }

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
data GameStateInput = GameStateInput
    { gsiPlanetInputs :: [PlanetInput]
    , gsiFleets  :: [Fleet]
    } deriving (Show)

data GameState = GameState {
    -- Planet stuff
      gsPlanets :: IntMap Planet
    , gsPlanetCount :: Int

    , gsPlanetsByOwner :: IntMap (IntMap Planet)
    , gsNeutralPlanets :: IntMap Planet
    , gsAlliedPlanets :: IntMap Planet
    , gsNonAlliedPlanets :: IntMap Planet
    , gsHostilePlanets :: IntMap Planet

    , gsDistances :: IntMap (IntMap Int)
    , gsDistanceById :: Int -> Int -> Int

    , gsProductionByOwner :: IntMap Int
    , gsAlliedProduction :: Int
    , gsHostileProduction :: Int

    -- Fleet stuff
    , gsFleets :: [Fleet]
    , gsFleetCount :: Int

    , gsFleetsByOwner :: IntMap [Fleet]
    , gsAlliedFleets :: [Fleet]
    , gsHostileFleets :: [Fleet]

    -- Combined info
    , gsShipsByOwner :: IntMap Int
    , gsAlliedShips :: Int
    , gsHostileShips :: Int
    }

makeGameState :: GameStateInput
              -> GameState
makeGameState gsi = GameState
    { gsPlanets = planets
    , gsPlanetCount = planetCount

    , gsPlanetsByOwner = planetsByOwner
    , gsNeutralPlanets = neutralPlanets
    , gsAlliedPlanets = alliedPlanets
    , gsNonAlliedPlanets = nonAlliedPlanets
    , gsHostilePlanets = hostilePlanets

    , gsDistances = distances
    , gsDistanceById = distanceById

    , gsProductionByOwner = productionByOwner
    , gsAlliedProduction = alliedProduction
    , gsHostileProduction = hostileProduction

    , gsFleets = fleets
    , gsFleetCount = fleetCount

    , gsFleetsByOwner = fleetsByOwner
    , gsAlliedFleets = alliedFleets
    , gsHostileFleets = hostileFleets

    , gsShipsByOwner = shipsByOwner
    , gsAlliedShips = alliedShips
    , gsHostileShips = hostileShips
    }
  where
    planets = IM.fromList . zipWith ((.) <$> (,) <*> makePlanet) [0..] . reverse
            $ gsiPlanetInputs gsi
    planetCount = IM.size planets

    ~( planetsByOwner, neutralPlanets, alliedPlanets, nonAlliedPlanets
     , hostilePlanets
     ) = pdoHelper planets

    distances = IM.map distancesFrom planets
      where
        distancesFrom p = IM.map (ceiling . distanceBetween p) planets
    distanceById = (IM.!) . (distances IM.!)

    productionByOwner = IM.map (F.sum . IM.map planetGrowthRate) planetsByOwner
    (_, aprod, hprod) = IM.splitLookup 1 productionByOwner
    alliedProduction = maybe 0 id aprod
    hostileProduction = F.sum hprod

    fleets = gsiFleets gsi
    fleetCount = length fleets

    ~(fleetsByOwner, alliedFleets, hostileFleets) = fboHelper fleets

    ~(shipsByOwner, alliedShips, hostileShips) =
        sboHelper planetsByOwner fleetsByOwner

pdoHelper planets = ( planetsByOwner, neutralPlanets, alliedPlanets
                    , nonAlliedPlanets , hostilePlanets
                    )
  where
    accum pid p = IM.insertWith IM.union (planetOwner p) $ IM.singleton pid p
    planetsByOwner = IM.foldWithKey accum IM.empty planets
    (np, ap, hp) = IM.splitLookup 1 planetsByOwner
    neutralPlanets = IM.fold IM.union IM.empty np
    alliedPlanets = maybe IM.empty id ap
    nonAlliedPlanets = neutralPlanets `IM.union` hostilePlanets
    hostilePlanets = IM.fold IM.union IM.empty hp

fboHelper fleets = (fleetsByOwner, alliedFleets, hostileFleets)
  where
    accum f = IM.insertWith (++) (fleetOwner f) [f]
    fleetsByOwner = foldl' accum IM.empty fleets
    (_, af, hf) = IM.splitLookup 1 fbo
    alliedFleets = maybe [] id af
    hostileFleets = F.concat hf

sboHelper pbo fbo = (shipsByOwner, alliedShips, hostileShips)
  where
    planetShipsByOwner = IM.map (F.sum . IM.map planetShips) pbo
    fleetShipsByOwner = IM.map (sum . map fleetShips) fbo
    shipsByOwner = IM.unionWith (+) planetShipsByOwner fleetShipsByOwner
    (_, aships, hships) = IM.splitLookup 1 sbo
    alliedShips = maybe 0 id aships
    hostileShips = F.sum hships

-- | Find planet in GameState with given planetId
--
getPlanetById :: Int -> GameState -> Planet
getPlanetById pid state = (IM.!) (gsPlanets state) pid

-- | Auxiliary function for parsing the game state. This function takes an
-- initial state, and a line. The line is parsed and content is applied on the
-- given state. Folding with this function can produce the entire game state.
--
buildGameState :: GameStateInput -- ^ Initial game state
               -> String         -- ^ Line to parse and apply
               -> GameStateInput -- ^ Resulting game state
buildGameState input string = case words string of
    ("P" : strX : strY : strOwner : strShips : strGrowthRate : _) ->
        let planetInput = PlanetInput (read strOwner)
                                      (read strShips)
                                      (read strGrowthRate)
                                      (read strX)
                                      (read strY)
        in input { gsiPlanetInputs = planetInput : gsiPlanetInputs input
                 }
    ("F" : strOwner : strShips : strSrc : strDest : strLen : strRem : _) ->
        let fleet = Fleet (read strOwner)
                          (read strShips)
                          (read strSrc)
                          (read strDest)
                          (read strLen)
                          (read strRem)
        in input { gsiFleets = fleet : gsiFleets input
                 }
    _ -> input

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
isNeutral = (< 1) . owner

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
        lgs { gameStatePlanets = IM.map setNeutral $ gsPlanets lgs
            , gameStateFleets  = filter ((/= player) . fleetOwner)
                               $ gsFleets lgs
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
            then let
                newPlanet = sourcePlanet { planetShips = sourceShips - movingShips }
                newFleet = Fleet
                    { fleetOwner = p
                    , fleetShips = movingShips
                    , fleetSource = sourceId
                    , fleetDestination = destinationId
                    , fleetTripLength = tripLength
                    , fleetTurnsRemaining = tripLength
                    }
            in lgs
                { gsPlanets = IM.insert sourceId newPlanet $ gsPlanets lgs
                , gsFleets = newFleet : gsFleets lgs
                }
            else lgs

-- | Perform the 'Departure' phase, but do not report what players are
-- | dropped.  Those players are still dropped, it just isn't reported.
--
departureNoFailReport :: IntMap [Order] -- ^ Orders, grouped by issuing player
                      -> GameState      -- ^ Old game state
                      -> GameState      -- ^ New game state
departureNoFailReport = (snd .) . departure

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
                                       $ gsPlanets gs
                    , gameStateFleets = map advanceFleet $ gsFleets gs
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
                      `IM.union` gsPlanets gs
                , gameStateFleets  = remainingFleets
                }
  where
    -- Make a fleet representing the defence forces on a planet.
    planetsAndFleets = IM.map ((,) <$> id <*> (IM.singleton <$> planetOwner
                                                            <*> planetShips))
                     $ gsPlanets gs

    -- Pull out arriving fleets for processing leaving fleets still in
    -- transit for later.
    (arrivingFleets, remainingFleets) = partition ((== 0) . fleetTurnsRemaining)
                                      $ gsFleets gs

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
engineTurn ordersMap gs = ( ( gameOver, winner, losers ), gs' )
  where
    -- Who started the turn?
    (_, planetPlayers) = IS.split 0 . IM.keysSet $ gsPlanetsByOwner gs
    fleetPlayers = IM.keysSet $ gsFleetsByOwner gs
    startingPlayers = planetPlayers `IS.union` fleetPlayers

    -- Execute all phases
    gs' = arrival . advancement $ departureNoFailReport ordersMap gs

    -- Who is left after that?
    (_, planetPlayers') = IS.split 0 . IM.keysSet $ gsPlanetsByOwner gs'
    fleetPlayers' = IM.keysSet $ gsFleetsByOwner gs'
    remainingPlayers = planetPlayers' `IS.union` fleetPlayers'

    -- Find the losers
    losers = IS.elems $ startingPlayers IS.\\ remainingPlayers

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

setOwner :: Int
         -> Planet
         -> Planet
setOwner o p = p { planetOwner = o }

-- | Set the number of ships on a planet
--
setShips :: Int
         -> Planet
         -> Planet
setShips n p = p { planetShips = n }

-- | Add a number of ships to a planet
--
addShips :: Int     -- ^ Number of ships to add
         -> Planet  -- ^ Planet to add ships to
         -> Planet  -- ^ Resulting planet
addShips n p = p { planetShips = planetShips p + n }

-- | Remove ships from a planet
--
removeShips :: Int    -- ^ Ships to remove
            -> Planet -- ^ Planet from which to remove
            -> Planet -- ^ Resulting planet
removeShips n p = p { planetShips = planetShips p - n }

-- | Find the distance between two planets
--
distanceBetween :: Planet -> Planet -> Double
distanceBetween p1 p2 = let dx = planetX p1 - planetX p2
                            dy = planetY p1 - planetY p2
                        in sqrt $ dx * dx + dy * dy

-- | Check if a fleet has arrived
--
isArrived :: Fleet -> Bool
isArrived = (== 0) . fleetTurnsRemaining

-- | Get a planet by ID. Make sure the ID exists!
--
planetById :: GameState -> Int -> Planet
planetById state id' = fromJust $ IM.lookup id' $ gsPlanets state

stepAllFleets :: GameState -> GameState
stepAllFleets state | null (gsFleets state) = state
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
    catch (loop emptyGsi) $ \e -> if isEOFError e
        then return ()
        else ioError e
  where
    emptyGsi = GameStateInput
        { gsiPlanetInputs = []
        , gsiFleets = []
        }
    loop input = do
        line <- takeWhile (/= '#') <$> getLine
        if "go" `isPrefixOf` line
            -- Go Go Go!
            then do
                f $ makeGameState input
                finishTurn
                loop mempty
            -- Keep building map
            else loop (buildGameState input line)

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
inputFromFile :: FilePath          -- ^ Path to the file containing the game state.
              -> IO GameStateInput -- ^ Parsed game state
inputFromFile path = withFile path ReadMode (loop mempty)
  where
    loop input handle = do
      line <- takeWhile (/= '#') <$> hGetLine handle
      if "go" `isPrefixOf` line
        then return input
        else loop (buildGameState input line) handle

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
currentOwner state pid = owner $ gsPlanets state IM.! pid

-- | List of planets under attack, i.e., that have incoming fleets.
--
planetsUnderAttack :: GameState -- ^ Game state to analyze
                   -> [Int]     -- ^ List of IDs of planets under attack
planetsUnderAttack = (map fleetDestination) . gsFleets

-- | List of incoming fleets for a given planet in a certain game state.
--
incomingFleets :: GameState -- ^ Game state containing the current fleets
               -> Int       -- ^ Planet ID
               -> [Fleet]   -- ^ Incoming fleets
incomingFleets state pid = filter pidMatches fleets
  where
    pidMatches = (== pid) . fleetDestination
    fleets = gsFleets state

-- ex: set sw=4 et ts=4 sts=4: Vim modeline
