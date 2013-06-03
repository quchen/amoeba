-- TODO: Make sure the node doesn't connect to itself
-- TODO: Lots of status/logging messages
-- TODO: The wire protocol uses Int64 length headers. Make the program robust
--       against too long messages that exceed the size. Maybe use
--       hGetContents after all?
-- TODO: Maintain last signal from an upstream neighbour, and send keep-alive
--       signals downstream every now and then

-- NOT TO DO: Rewrite all the NodeState parameters to Reader.
--       RESULT: Don't do it, the whole code is littered with liftIOs.


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE CPP #-}

-- Until feature complete:
#define errorCPP(x) error ("Error: Line " ++ show __LINE__ ++ ": " ++ x)

-- {-# OPTIONS_GHC -ddump-splices #-} -- For Lens.TH debugging

module Main where

import GHC.Generics (Generic)
import Data.Word
import Data.Int
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import System.IO
import System.Random
import Control.Concurrent.STM
import Network
import Control.Applicative
import Data.Functor
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS
import Text.Printf
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Lens
import Data.Maybe

import Data.Binary








data Config = Config {

        _maxNeighboursL  :: Word       -- ^ The maximum number of neighbours. No
                                       --   new ones will be accepted once it's
                                       --   full.

      , _minNeighboursL  :: Word       -- ^ The minimum number of neighbours. If
                                       --   the current number is smaller issue
                                       --   announce signals.

      , _portRangeL      :: (Int, Int) -- ^ The node will open a server on a
                                       --   randomly picked node in this range.

      , _maxChanSizeL    :: Int        -- ^ How many entries the bounded
                                       --   communication channels can hold

      , _maxRandomPortsL :: Word       -- ^ Number of retries to find a random
                                       --   open port

      , _bouncesL        :: Word       -- ^ Number of initial bounces
      , _lambdaL         :: Double     -- ^ Parameter for exponentially
                                       --   distributed things. Each step, the
                                       --   probability will decrease by a
                                       --   factor lambda. Must be >= 1.
      , _poolTickRateL   :: Int        -- ^ Every couple of milliseconds, the
                                       --   client pool will loop to maintain a
                                       --   proper connection to the network.
      , _poolTimeoutL    :: Double     -- ^ Number of seconds before a
                                       --   non-responding node is considered
                                       --   gone
      }

makeLenses ''Config

-- | Node configuration. Hardcoded because it's easier for the time being.
config = Config {
        _maxNeighboursL  = 10
      , _minNeighboursL  = 5
      , _portRangeL      = (20000, 20100)
      , _maxChanSizeL    = 100
      , _maxRandomPortsL = 10
      , _bouncesL        = 3
      , _lambdaL         = 1.5
      , _poolTickRateL   = 1 * 10^6
      , _poolTimeoutL    = 10
      }





-- | Uniquely identifies a node in a network by providing the address of its
--   server.
data Node = Node { _hostL :: HostName
                 , _portL :: PortNumber
                 }
                 deriving (Eq, Ord, Show, Generic)

instance Binary PortNumber where
      get = (fromIntegral :: Int -> PortNumber) <$> get
      put = put . (fromEnum :: PortNumber -> Int)

instance Binary Node
makeLenses ''Node



newtype Timestamp = Timestamp Double
      deriving (Eq, Ord, Show, Generic)

instance Binary Timestamp

-- | Creates a timestamp, which is a Double representation of the Unix time.
makeTimestamp :: IO Timestamp
makeTimestamp = Timestamp . realToFrac <$> getPOSIXTime
--   Since Haskell's Time library is borderline retarded, this seems to be the
--   cleanest way to get something that is easily an instance of Binary and
--   comparable to seconds.




-- | Direction of a query that establishes a new connection
data Direction = Request  -- ^ Request new neighbours to fill the pool
               | Announce -- ^ Announce a certain node so others add it to their
                          --   pool
               deriving (Eq, Ord, Show, Generic)

instance Binary Direction





-- | Signifies a question, or a positive/negative answer to one.
data Predicate = Question | Yes | No
      deriving (Eq, Ord, Show, Generic)

instance Binary Predicate




-- | Used in loops that may end. Continue means looping, Terminate hops out.
data Proceed = Continue | Terminate
      deriving (Eq, Ord, Show)

-- | Similar to @Control.Monad.forever@, but will abort when @False@ is
--   returned.
untilTerminate :: Monad m => m Proceed -> m ()
untilTerminate m = go
      where go = m >>= \case Continue  -> go
                             Terminate -> return ()




data EdgeData = EdgeData {
        _directionL   :: Direction
      , _bounceParamL :: (Either Word Double)
      }
      deriving (Eq, Ord, Show, Generic)

instance Binary EdgeData
makeLenses ''EdgeData





-- | Stores a signal to be executed by a node, e.g. print a message, search for
--   new neighbours etc.
data Signal =

        -- | Query to add an edge to the network. Direction specifies which way
        --   the new connection should go. The Either part is used to limit how
        --   far the request bounces through the network.
        EdgeRequest (Either PortNumber Node) EdgeData

        -- | Sent to the new downstream neighbour node so it can keep track of
        --   how many times it's referenced
      | IAddedYou

        -- | Sent to the requesting node: "I have upstream neighbour space free"
      | AddMe

        -- | This node is shutting down, remove it from your neighboured-by pool
      | ShuttingDown

        -- | Text message. Should only be considered when it comes timestamped
        --   in a SignalT.
      | Message String

      deriving (Eq, Ord, Show, Generic)

instance Binary Signal
makePrisms ''Signal




-- | A tuple of timestamp, issuing node, signal.
data SignalT = SignalT {
        _timestampL :: Maybe Timestamp
      , _signalL    :: Signal
      }
      deriving (Eq, Ord, Show, Generic)
makeLenses ''SignalT

instance Binary SignalT





-- | State of the local node
data NodeState = NodeState {
        _knownNodesL :: TVar (Set Node) -- ^ Neighbours this node knows
      , _knownByL    :: TVar (Map Node Timestamp) -- ^ Nodes this node is a
                                                  -- neighbour of, plus the time
                                                  -- the last signal was
                                                  -- received from it.
      , _stcL        :: TChan SignalT   -- ^ Send messages to all clients
      , _st1cL       :: TBQueue SignalT -- ^ Send message to one client
      , _ioL         :: TBQueue (IO ()) -- ^ Send action to the dedicated
                                          -- local IO thread
      , _handledQueriesL :: TVar (Set SignalT)
                                        -- ^ Timestamped signals that have
                                        --   already been handled by this
                                        --   node, and can thus be ignored if
                                        --   they come in again.
      , _myPortL     :: PortNumber      -- ^ Own port
      }

makeLenses ''NodeState





main = startNode





-- | Node main function
startNode :: IO ()
startNode = bracket (randomSocket $ config ^. maxRandomPortsL)
                    sClose $
                    \socket -> do

      ~(PortNumber myPort) <- socketPort socket
      ns <- initState myPort

      -- Start server loop
      withAsync (serverLoop socket ns) $ \server -> do
            forkIO $ localIO (ns ^. ioL) -- Dedicated IO thread
            forkIO $ clientPool ns -- Client pool
            wait server





-- | Initializes this node's state
initState :: PortNumber -> IO NodeState
initState port = NodeState
        <$> newTVarIO Set.empty -- Known nodes
        <*> newTVarIO Map.empty -- Nodes known by plus last signal timestamps
        <*> newBroadcastTChanIO -- Channel to all clients
        <*> newTBQueueIO size   -- Channel to one client
        <*> newTBQueueIO size   -- Channel to the IO thread
        <*> newTVarIO Set.empty -- Previously handled queries
        <*> pure port           -- Own server's port
        where size = config ^. maxChanSizeL
              portErr = error "Own port unknown. This is a bug."






-- | Used for a dedicated IO thread. Reads IO actions from a queue and executes
--   them.
--
--   > localIO q = forever $ do action <- atomically $ readTBQueue q
--   >                          action
localIO :: TBQueue (IO ()) -> IO ()
localIO = forever . join . atomically . readTBQueue





-- #############################################################################
-- ########  CLIENTS  ##########################################################
-- #############################################################################




-- Initializes a new client, and then spawns the client loop.
newClient :: NodeState -> Node -> IO ()
newClient ns node = do

      -- Duplicate broadcast chan for the new client
      stc <- atomically $ dupTChan (ns ^. stcL)
      let st1c = ns ^. st1cL

      -- Open connection
      bracket (connectTo (node ^. hostL) (node ^. portL . to PortNumber))
              hClose
              (\h -> clientLoop ns h stc st1c)




-- Listens to a TChan (signals broadcast to all nodes) and a TBQueue (signals
-- meant to be handled by only one client), and executes their orders.
clientLoop :: NodeState -> Handle -> TChan SignalT -> TBQueue SignalT -> IO ()
clientLoop ns h stc st1c = untilTerminate $ do

      -- Receive orders from whatever channel is first available
      signalT <- atomically $ msum [readTChan stc, readTBQueue st1c]

      -- Listen to the order channels and execute them
      case signalT ^. signalL of
            Message _   -> send h signalT
            -- TODO: Implement other signals
            _otherwise  -> atomically $ writeTBQueue (ns ^. ioL) $
                  putStrLn $ "Error: The signal " ++ show signalT ++ " should"
                                        ++ " never have been sent to a client"
                                        ++ errorCPP("Implement other signals")


      -- TODO: Termination
      --   - If the handle is closed, remove the client from the client pool and
      --     terminate.
      --   - If the client is not in the pool, close the connection. Send a
      --     termination signal to the targeted node so it can adjust its
      --     knownBy set accordingly
      return Continue





-- | Sends a SignalT, encoded as Binary with a size header, to a Handle.
--   Inverse of 'receive'.
send :: Handle -> SignalT -> IO ()
send h signalT = do
      let sBinary = encode signalT
          sLength = encode (BS.length sBinary :: Int64)
      BS.hPut h $ sLength
      BS.hPut h $ sBinary
      hFlush h





-- #############################################################################
-- ########  CLIENT POOL  ######################################################
-- #############################################################################




-- | The client pool is a loop that makes sure the client count isn't too low.
--   It does this by periodically checking the current values, and issuing
--   announces/requests if necessary. The actual client threads will be spawned
--   by the server when it receives an according signal.
--
--   The goal is to know and be known by the minimum amount of nodes specified
--   by the configuration.
clientPool :: NodeState -> IO ()
clientPool ns = forever $ do

      -- Cleanup: Remove all nodes from the knownBy pool that haven't sent a
      -- signal in some time
      (Timestamp now) <- makeTimestamp
      atomically $ do
            let notTimedOut (Timestamp t) = now - t < config ^. poolTimeoutL -- TODO: check whether the </- are right :-)
            modifyTVar (ns ^. knownByL) (Map.filter notTimedOut)

      -- How many nodes does this node know, how many is it known by?
      (numKnownNodes, numKnownBy) <- atomically $ liftM2 (,)
            (fromIntegral . Set.size <$> readTVar (ns ^. knownNodesL))
            (fromIntegral . Map.size <$> readTVar (ns ^. knownByL   ))
            -- ^ :: Int -> Word

      let minNeighbours = config ^. minNeighboursL

      case numKnownNodes of
            0 -> bootstrap -- TODO
            _ | numKnownNodes < minNeighbours -> do  -- Send out requests
                  let deficit = minNeighbours - numKnownNodes
                  forM_ [1..deficit] $ \_ -> sendEdgeRequest ns Request
            _ -> return () -- Otherwise: no deficit, do nothing

      when (numKnownBy < minNeighbours) $ do
            -- Send out announces
            let deficit = minNeighbours - numKnownBy
            forM_ [1..deficit] $ \_ -> sendEdgeRequest ns Announce

      threadDelay $ config ^. poolTickRateL



-- | Sends out a request for either an incoming (announce) or outgoing (request)
--   edge to the network.
sendEdgeRequest :: NodeState -> Direction -> IO ()
sendEdgeRequest ns signalType = do
      let n = config ^. bouncesL
      time <- Just <$> makeTimestamp
      atomically $ writeTBQueue (ns ^. st1cL) $
            SignalT time .
            EdgeRequest (Left $ ns ^. myPortL) .
            EdgeData signalType $
            Left n


-- TODO
bootstrap = errorCPP("Bootstrap")




-- #############################################################################
-- ########  SERVER  ###########################################################
-- #############################################################################





-- | Tries opening a socket on a certain amount of random ports.
randomSocket :: Word -> IO Socket
randomSocket 0 = error "Couldn't find free port"
randomSocket n = do
      socket <- randomPort >>= try . listenOn
      case socket :: Either SomeException Socket of
            Left  _ -> randomSocket (n-1)
            Right r -> return r





-- | Generates a random PortID based on the 'portRange' config value
randomPort :: IO PortID
randomPort = PortNumber . fromIntegral <$> randomRIO (config ^. portRangeL)





-- | Forks off a worker for each incoming connection.
serverLoop :: Socket -> NodeState -> IO ()
serverLoop socket ns = forever $ do

      -- Accept incoming connections
      connection@(h, _, _) <- accept socket
      hSetBinaryMode h True
      forkIO $ worker connection ns





-- | Handles an incoming connection: Pass incoming work orders on to clients,
--   print chat messages etc.
--   (The first parameter is the same as in the result of Network.accept.)
worker :: (Handle, HostName, PortNumber) -> NodeState -> IO ()
worker (h, host, port) ns = untilTerminate $ do

      let clientNode = Node host port

      -- TODO: Error handling: What to do if rubbish data comes in?
      signalT <- receive h

      case signalT ^. signalL of
            Message {}         -> floodMessage ns signalT
            ShuttingDown       -> shuttingDown ns clientNode
            IAddedYou          -> iAddedYou ns clientNode
            AddMe              -> error("Implement addMe")
            EdgeRequest {}     -> edgeBounce ns (fillInHost host signalT)

      -- Update "last heard of" timestamp. (Will not do anything if the node
      -- isn't in the list.)
      makeTimestamp >>= atomically . updateTimestamp ns clientNode

      return Continue





-- | A node doesn't know its own hostname (only its port) when sending off a
--   request that must come back somehow (e.g. EdgeRequest). For this reason, it
--   only attaches its port, and leaves entering the hostname to the node it
--   sends the information first to.
--
--   If there's already a full node saved in it, it won't do anything.
fillInHost :: HostName -> SignalT -> SignalT
fillInHost host (SignalT timestamp (EdgeRequest (Left port) edgeData)) =
      SignalT timestamp (EdgeRequest (Right node) edgeData)
      where node = Node { _hostL = host, _portL = port }
fillInHost _ signalT = signalT





-- | Receives a SignalT, encoded as Binary with a size header, from a Handle.
--   Inverse of 'send'.
receive :: Handle -> IO SignalT
receive h = do
      let int2int = fromIntegral :: Int64 -> Int
          int64Size = BS.length $ encode (0 :: Int64)
      -- Read length of the data first
      sLength <- decode <$> BS.hGet h (int2int int64Size)
      -- Read the previously determined amount of data
      decode <$> BS.hGet h (int2int sLength)





-- | Sends a message to the printer thread
floodMessage :: NodeState -> SignalT -> IO Proceed
floodMessage ns signalT = do

      -- Only process the message if it has a timestamp and hasn't been
      -- processed already
      process <- atomically $ do
            handled <- Set.member signalT <$> readTVar (ns ^. handledQueriesL)
            return $ not handled && isJust (signalT ^. timestampL)

      when process $ atomically $ do

            -- Add signal to the list of already handled ones
            modifyTVar (ns ^. handledQueriesL) (Set.insert signalT)

            -- Print message on this node
            writeTBQueue (ns ^. ioL) $ putStrLn (signalT ^. signalL._Message)

            -- Propagate message on to all clients
            writeTChan (ns ^. stcL) signalT

      return Continue -- Keep the connection alive, e.g. to get more messages
                      -- from that node.





-- | Updates the timestamp in the "last heard of" database (if present).
updateTimestamp :: NodeState -> Node -> Timestamp -> STM ()
updateTimestamp ns node timestamp = modifyTVar (ns ^. knownByL) $
       (Map.adjust (const timestamp) node)





-- | When received, remove the issuing node from the database to ease network
--   cleanup.
shuttingDown :: NodeState -> Node -> IO Proceed
shuttingDown ns node = atomically $ do

      -- Status message
      let action = printf "Shutdown notice from %s:%s"
                          (node ^. hostL.to show)
                          (node ^. portL.to show)
      writeTBQueue (ns ^. ioL) action

      -- Remove from lists of known nodes and nodes known by
      modifyTVar (ns ^. knownByL) (Map.delete node)

      return Terminate -- The other node is shutting down, there's no need to
                       -- maintain a worker for it.





---- | Another node wants to know whether it still is this node's neighbour, so it
----   can keep track of how many times it is referenced.
----
----   Responds to the same handle as the incoming request.
--amIYourNeighbour :: NodeState -> Node -> Handle -> Predicate -> IO Proceed

---- Positive answer: Update last signal entry
--amIYourNeighbour ns node _h Yes = do
--      timestamp <- makeTimestamp
--      atomically $ modifyTVar (ns ^. knownByL) (Map.insert node timestamp)
--      return Continue -- The incoming connection from a node should not be
--                      -- affected by whether it is a neighbour of this node.

---- Negative answer: Remove node from database
--amIYourNeighbour ns node _h No = atomically $ do
--      modifyTVar (ns ^. knownByL) (Map.delete node)
--      return Continue -- Dito above.

--amIYourNeighbour ns node h Question = do
--      p <- atomically $ Set.member node <$> readTVar (ns ^. knownNodesL)
--      let response = AmIYourNeighbour (if p then Yes else No)
--      BS.hPut h $ encode response
--      return Continue -- Dito above.





-- | A node signals that it has added the current node to its pool. This happens
--   at the end of a neighbour search.
--
--   This should be the first signal this node receives from another node
--   choosing it as its new neighbour.
iAddedYou :: NodeState -> Node -> IO Proceed
iAddedYou ns node = do
      timestamp <- makeTimestamp
      atomically $ do
            modifyTVar (ns ^. knownByL) (Map.insert node timestamp)
            writeTBQueue (ns ^. ioL) $
                  putStrLn $ "New upstream neighbour: " ++ show node
      return Continue -- Let's not close the door in front of our new friend :-)





-- | A node signals that it's ready to have another upstream neighbour added,
--   and gives this node the permission to do so.
addMe :: NodeState -> Node -> IO Proceed
addMe ns node = error("Implement addMe")





-- TODO: Hardcore refactoring of this function using Lens magic
-- | This models the "bouncing" behaviour of finding neighbours. When a node
--   receives an announcement ("Hey, I'm here, please make me your neighbour")
--   or a request ("I need more neighbours"), it sends a single request to one
--   of its neighbours.
--
--   This request is then passed on - it "bounces" off that
--   node - and the process is repeated using two phases:
--
--     1. In phase 1, a counter will keep track of how many bounces have
--        occurred. For example, a signal may contain the information "bounce
--        me 5 times". This makes sure the signal traverses the network a
--        certain amount, so the signal doesn't just stay in the issuing node's
--        neighbourhood.
--
--     2. Phase 2 is like phase 1, but instead of a counter, there's a denial
--        probability. If there's room and the node rolls to keep the signal,
--        it will do what its contents say. If there is no room or the node
--        rolls to deny the request, it is bounced on once again, but with a
--        reduced denial probability. This leads to an approximately
--        exponentially distributed bounce-on-length in phase 2. This solves the
--        issue of having a long chain of nodes, where only having phase one
--        would reach the same node every time.
--
edgeBounce :: NodeState -> SignalT -> IO Proceed

-- Phase 1: Left value, bounce on.
edgeBounce ns (SignalT timestamp
                    (EdgeRequest origin
                          (EdgeData dir (Left n)))) = do

      let buildSignalT = SignalT timestamp . EdgeRequest origin . EdgeData dir
      atomically $ writeTBQueue (ns ^. st1cL) $ case n of
            0 -> buildSignalT . Right $ 1 / config ^. lambdaL
            k -> buildSignalT . Left  $ k - 1

      return Continue

-- Phase 2: either accept or bounce on with adjusted acceptance
-- probability.
--
-- (Note that bouncing on always decreases the denial probability, even in case
-- the reason was not enough room.)
edgeBounce ns (SignalT timestamp
                    (EdgeRequest origin
                          (EdgeData dir (Right p)))) = do

      -- "Bounce on" action with denial probabillity decreased by lambda
      let buildSignalT = SignalT timestamp . EdgeRequest origin . EdgeData dir
          bounceOn = atomically $ writeTBQueue (ns ^. st1cL) $
                buildSignalT . Right $ p / config ^. lambdaL

      -- Checks whether there's still room for another entry. The TVar can
      -- either be the set of known or "known by" nodes.
      let threshold = config ^. maxNeighboursL
          isRoomIn tVar sizeF = atomically $
                (< threshold) . fromIntegral . sizeF <$> readTVar tVar

      -- Roll whether to accept the query first, then check whether there's
      -- room. In case of failure, bounce on.
      accept <- (> p) <$> randomRIO (0,1)
      case (accept, dir) of
            (False, _) -> bounceOn
            (True, Request) -> do
                  isRoom <- isRoomIn (ns ^. knownNodesL) Set.size
                  if isRoom then do -- TODO: Accept. Spawn client, send clientAdded message.
                                  errorCPP("Accept, spawn client, send ClientAdded")
                            else bounceOn
            (True, Announce) -> do
                  isRoom <- isRoomIn (ns ^. knownByL) Map.size
                  if isRoom then do -- TODO: Accept. Send addMe approval.
                                  errorCPP("Accept, send AddMe")
                            else bounceOn

      return Continue
