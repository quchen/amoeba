-- TODO: Refactoring :-)

-- TODO: Make sure the node doesn't connect to itself
-- TODO: Lots of status/logging messages
-- TODO: The wire protocol uses Int64 length headers. Make the program robust
--       against too long messages that exceed the size. Maybe use
--       hGetContents after all?
-- TODO: Send keep-alive signals downstream every now and then
-- TODO: Create "network snapshot" type message to generate GraphViz pictures
--       of how everything looks like
-- TODO: Randomly replace downstream neighbours
-- TODO: Split into multiple modules
-- TODO: Split tasks up more. For example, the "remove timed out upstream nodes"
--       should become its own thread.

-- NOT TO DO: Rewrite all the NodeEnvironment parameters to Reader.
--       RESULT: Don't do it, the whole code is littered with liftIOs. The
--               explicit "ns" every time isn't so bad compared to it.


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE CPP #-}

-- Until feature complete:
#define errorCPP(x) error ("Error: Line " ++ show __LINE__ ++ ": " ++ x)

-- {-# OPTIONS_GHC -ddump-splices #-} -- For Lens.TH debugging
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}



module Main where



-- Base/Platform
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Lazy as BS
import           Data.Int
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Word
import           GHC.Generics (Generic)
import           Network
import           System.IO
import           System.Random
import           Text.Printf

-- Hackage
import           Data.Binary








data Config = Config {

        _maxNeighbours  :: Word       -- ^ The maximum number of neighbours. No
                                      --   new ones will be accepted once it's
                                      --   full.

      , _minNeighbours  :: Word       -- ^ The minimum number of neighbours. If
                                      --   the current number is smaller issue
                                      --   announce signals.

      , _portRange      :: (Int, Int) -- ^ The node will open a server on a
                                      --   randomly picked node in this range.

      , _maxChanSize    :: Int        -- ^ How many entries the bounded
                                      --   communication channels can hold

      , _maxRandomPorts :: Word       -- ^ Number of retries to find a random
                                      --   open port

      , _bounces        :: Word       -- ^ Number of initial bounces
      , _lambda         :: Double     -- ^ Parameter for exponentially
                                      --   distributed things. Each step, the
                                      --   probability will decrease by a
                                      --   factor lambda. Must be >= 1.
      , _poolTickRate   :: Int        -- ^ Every couple of milliseconds, the
                                      --   client pool will loop to maintain a
                                      --   proper connection to the network.
      , _keepAliveTickRate :: Int     -- ^ Like the pool tickrate, but for
                                      --   sending KeepAlive signals downstream.
      , _poolTimeout    :: Double     -- ^ Number of seconds before a
                                      --   non-responding node is considered
                                      --   gone
      }


-- | Node configuration. Hardcoded because it's easier for the time being.
config :: Config
config = Config {
        _maxNeighbours     = 10
      , _minNeighbours     = 5
      , _portRange         = (20000, 20100)
      , _maxChanSize       = 100
      , _maxRandomPorts    = 10
      , _bounces           = 3
      , _lambda            = 1.5
      , _poolTickRate      = 1 * 10^6
      , _keepAliveTickRate = 1 * 10^6
      , _poolTimeout       = 10
      }





-- | Uniquely identifies a node in a network by providing the address of its
--   server.
data Node = Node { _host :: HostName
                 , _port :: PortNumber
                 }
                 deriving (Eq, Ord, Show, Generic)

instance Binary PortNumber where
      get = (fromIntegral :: Int -> PortNumber) <$> get
      put = put . (fromEnum :: PortNumber -> Int)

instance Binary Node



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
        _direction   :: Direction
      , _bounceParam :: (Either Word Double)
      }
      deriving (Eq, Ord, Show, Generic)

instance Binary EdgeData





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

        -- | Randomly sent to downstream nodes so the timestamps are refreshed,
        --   and the node is kept in the books as an upstream neighbour
      | KeepAlive

        -- | This node is shutting down, remove it from your neighboured-by pool
      | ShuttingDown

        -- | Text message. Should only be considered when it comes timestamped
        --   in a Signal.
      | Message Timestamp String

      deriving (Eq, Ord, Show, Generic)

instance Binary Signal





-- | State of the local node
data NodeEnvironment = NodeEnvironment {
        _knownNodes :: TVar (Set Node) -- ^ Neighbours this node knows
      , _knownBy    :: TVar (Map Node Timestamp)
                                       -- ^ Nodes this node is a
                                       -- neighbour of, plus the time
                                       -- the last signal was
                                       -- received from it.
      , _stc        :: TChan Signal    -- ^ Send messages to all clients
      , _st1c       :: TBQueue Signal  -- ^ Send message to one client
      , _io         :: TBQueue (IO ()) -- ^ Send action to the dedicated
                                       --   local IO thread
      , _handledQueries :: TVar (Set Signal)
                                       -- ^ Timestamped signals that have
                                       --   already been handled by this
                                       --   node, and can thus be ignored if
                                       --   they come in again.
      , _myPort     :: PortNumber      -- ^ Own port
      }






main :: IO ()
main = startNode





-- | Node main function
startNode :: IO ()
startNode = bracket (randomSocket $ _maxRandomPorts config)
                    sClose $
                    \socket -> do

      ~(PortNumber myPort) <- socketPort socket
      env <- initEnvironment myPort

      -- Start server loop
      withAsync (serverLoop socket env) $ \server -> do
            void . forkIO $ localIO (_io env) -- Dedicated IO thread
            void . forkIO $ clientPool env -- Client pool
            wait server



keepAliveLoop :: NodeEnvironment -> IO ()
keepAliveLoop env = forever $ do

      -- Send KeepAlive signal to a random downstream neighbour. This will
      -- case a little spam when there are initially very few downstream
      -- neighbours, but later on the least busy nodes are more likely to pick
      -- up the signal, conveniently favouring them to send KeepAlive signals.
      atomically $ writeTBQueue (_st1c env) KeepAlive

      -- Cleanup: Remove all nodes from the knownBy pool that haven't sent a
      -- signal in some time
      (Timestamp now) <- makeTimestamp
      atomically $ do
            -- TODO: check whether the </- are right :-)
            let notTimedOut (Timestamp t) = now - t < _poolTimeout config
            modifyTVar (_knownBy env) (Map.filter notTimedOut)

      threadDelay (_keepAliveTickRate config)





-- | Initializes this node's state
initEnvironment :: PortNumber -> IO NodeEnvironment
initEnvironment port = NodeEnvironment
        <$> newTVarIO Set.empty -- Known nodes
        <*> newTVarIO Map.empty -- Nodes known by plus last signal timestamps
        <*> newBroadcastTChanIO -- Channel to all clients
        <*> newTBQueueIO size   -- Channel to one client
        <*> newTBQueueIO size   -- Channel to the IO thread
        <*> newTVarIO Set.empty -- Previously handled queries
        <*> pure port           -- Own server's port
        where size = _maxChanSize config






-- | Used for a dedicated IO thread. Reads IO actions from a queue and executes
--   them.
--
--   > localIO q = forever $ do action <- atomically $ readTBQueue q
--   >                          action
localIO :: TBQueue (IO ()) -> IO ()
localIO = forever . join . atomically . readTBQueue





-- | Sends an IO action to the IO thread.
toIO :: NodeEnvironment -> IO () -> STM ()
toIO env = writeTBQueue (_io env)





-- #############################################################################
-- ########  CLIENTS  ##########################################################
-- #############################################################################




-- Initializes a new client, and then spawns the client loop.
newClient :: NodeEnvironment -> Node -> IO ()
newClient ne node = do

      -- Duplicate broadcast chan for the new client
      stc <- atomically $ dupTChan (_stc ne)
      let st1c = _st1c ne

      -- Open connection
      bracket (connectTo (_host node) (PortNumber $ _port node))
              hClose
              (\h -> clientLoop ne h stc st1c)




-- Listens to a TChan (signals broadcast to all nodes) and a TBQueue (signals
-- meant to be handled by only one client), and executes their orders.
clientLoop :: NodeEnvironment -> Handle -> TChan Signal -> TBQueue Signal -> IO ()
clientLoop env h stc st1c = untilTerminate $ do

      -- Receive orders from whatever channel is first available
      signal <- atomically $ msum [readTChan stc, readTBQueue st1c]

      -- Listen to the order channels and execute them
      case signal of
            Message {}   -> send h signal
            -- TODO: Implement other signals
            _otherwise  -> atomically $ toIO env $
                  putStrLn $ "Error: The signal " ++ show signal ++ " should"
                                        ++ " never have been sent to a client"
                                        ++ errorCPP("Implement other signals")


      -- TODO: Termination
      --   - If the handle is closed, remove the client from the client pool and
      --     terminate.
      --   - If the client is not in the pool, close the connection. Send a
      --     termination signal to the targeted node so it can adjust its
      --     knownBy set accordingly
      return Continue





-- | Sends a Signal, encoded as Binary with a size header, to a Handle.
--   Inverse of 'receive'.
send :: Handle -> Signal -> IO ()
send h signal = do
      let sBinary = encode signal
          sLength = encode (BS.length sBinary :: Int64)
      BS.hPut h $ sLength
      BS.hPut h $ sBinary
      hFlush h





-- #############################################################################
-- ########  CLIENT _POO  ######################################################
-- #############################################################################



-- | Sets up the client pool by forking the KeepAlive thread, and then starts
--   the client pool loop.
clientPool :: NodeEnvironment -> IO ()
clientPool env = forkIO (keepAliveLoop env) *> clientPoolLoop env


-- | The client pool is a loop that makes sure the client count isn't too low.
--   It does this by periodically checking the current values, and issuing
--   announces/requests if necessary. The actual client threads will be spawned
--   by the server when it receives an according signal.
--
--   The goal is to know and be known by the minimum amount of nodes specified
--   by the configuration.
clientPoolLoop :: NodeEnvironment -> IO ()
clientPoolLoop env = forever $ do

      -- How many nodes does this node know, how many is it known by?
      (numKnownNodes, numKnownBy) <- atomically $ liftM2 (,)
            (fromIntegral . Set.size <$> readTVar (_knownNodes env))
            (fromIntegral . Map.size <$> readTVar (_knownBy env   ))
            -- ^ :: Int -> Word

      let minNeighbours = _minNeighbours config

      -- Enough downstream neighbours?
      case numKnownNodes of
            0 -> bootstrap -- TODO
            _ | numKnownNodes < minNeighbours -> do  -- Send out requests
                  let deficit = minNeighbours - numKnownNodes
                  forM_ [1..deficit] $ \_ -> sendEdgeRequest env Request
            _ -> return () -- Otherwise: no deficit, do nothing

      -- Enough upstream neighbours?
      when (numKnownBy < minNeighbours) $ do
            -- Send out announces
            let deficit = minNeighbours - numKnownBy
            forM_ [1..deficit] $ \_ -> sendEdgeRequest env Announce

      threadDelay $ _poolTickRate config






-- | Sends out a request for either an incoming (announce) or outgoing (request)
--   edge to the network.
sendEdgeRequest :: NodeEnvironment -> Direction -> IO ()
sendEdgeRequest env signalype = do
      let n = _bounces config
      atomically $ writeTBQueue (_st1c env) $
            EdgeRequest (Left $ _myPort env) .
            EdgeData signalype $
            Left n


-- TODO
bootstrap :: a
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
randomPort = PortNumber . fromIntegral <$> randomRIO (_portRange config)





-- | Forks off a worker for each incoming connection.
serverLoop :: Socket -> NodeEnvironment -> IO ()
serverLoop socket env = forever $ do

      -- Accept incoming connections
      connection@(h, _, _) <- accept socket
      hSetBinaryMode h True
      forkIO $ worker connection env





-- | Handles an incoming connection: Pass incoming work orders on to clients,
--   print chat messages etc.
--   (The first parameter is the same as in the result of Network.accept.)
worker :: (Handle, HostName, PortNumber) -> NodeEnvironment -> IO ()
worker (h, host, port) env = untilTerminate $ do

      let clientNode = Node host port
      -- TODO: Ignore signals sent by nodes not registered as upstream.
      --       (Do this here or in the server loop?)

      -- Update "last heard of" timestamp. (Will not do anything if the node
      -- isn't in the list.)
      makeTimestamp >>= atomically . updateTimestamp env clientNode

      -- TODO: Error handling: What to do if rubbish data comes in?
      signal <- receive h

      case signal of
            Message {}         -> floodMessage env signal
            ShuttingDown       -> shuttingDown env clientNode
            IAddedYou          -> iAddedYou env clientNode
            AddMe              -> error("Implement addMe")
            EdgeRequest {}     -> edgeBounce env (fillInHost host signal)
            KeepAlive          -> return Continue -- Just update timestamp





-- | A node doesn't know its own hostname (only its port) when sending off a
--   request that must come back somehow (e.g. EdgeRequest). For this reason, it
--   only attaches its port, and leaves entering the hostname to the node it
--   sends the information first to.
--
--   If there's already a full node saved in it, it won't do anything.
fillInHost :: HostName -> Signal -> Signal
fillInHost host (EdgeRequest (Left port) edgeData) =
      EdgeRequest (Right node) edgeData
      where node = Node { _host = host, _port = port }
fillInHost _ signal = signal





-- | Receives a Signal, encoded as Binary with a size header, from a Handle.
--   Inverse of 'send'.
receive :: Handle -> IO Signal
receive h = do
      let int2int = fromIntegral :: Int64 -> Int
          int64Size = BS.length $ encode (0 :: Int64)
      -- Read length of the data first
      sLength <- decode <$> BS.hGet h (int2int int64Size)
      -- Read the previously determined amount of data
      decode <$> BS.hGet h (int2int sLength)





-- | Sends a message to the printer thread
floodMessage :: NodeEnvironment -> Signal -> IO Proceed
floodMessage env signal = do

      -- Only process the message if it hasn't been processed already
      process <- atomically $
            Set.member signal <$> readTVar (_handledQueries env)

      when process $ atomically $ do

            -- Add signal to the list of already handled ones
            modifyTVar (_handledQueries env) (Set.insert signal)

            -- Print message on this node
            let ~(Message _timestamp message) = signal
            toIO env $ putStrLn message

            -- Propagate message on to all clients
            writeTChan (_stc env) signal

      return Continue -- Keep the connection alive, e.g. to get more messages
                      -- from that node.





-- | Updates the timestamp in the "last heard of" database (if present).
updateTimestamp :: NodeEnvironment -> Node -> Timestamp -> STM ()
updateTimestamp env node timestamp = modifyTVar (_knownBy env) $
       (Map.adjust (const timestamp) node)





-- | When received, remove the issuing node from the database to ease network
--   cleanup.
shuttingDown :: NodeEnvironment -> Node -> IO Proceed
shuttingDown env node = atomically $ do

      -- Status message
      let action = printf "Shutdown notice from %s:%s"
                          (show $ _host node)
                          (show $ _port node)
      writeTBQueue (_io env) action

      -- Remove from lists of known nodes and nodes known by
      modifyTVar (_knownBy env) (Map.delete node)

      return Terminate -- The other node is shutting down, there's no need to
                       -- maintain a worker for it.







-- | A node signals that it has added the current node to its pool. This happens
--   at the end of a neighbour search.
--
--   This should be the first signal this node receives from another node
--   choosing it as its new neighbour.
iAddedYou :: NodeEnvironment -> Node -> IO Proceed
iAddedYou env node = do
      timestamp <- makeTimestamp
      atomically $ do
            modifyTVar (_knownBy env) (Map.insert node timestamp)
            toIO env $ putStrLn $ "New upstream neighbour: " ++ show node
      return Continue -- Let's not close the door in front of our new friend :-)





-- | A node signals that it's ready to have another upstream neighbour added,
--   and gives this node the permission to do so.
addMe :: NodeEnvironment -> Node -> IO Proceed
addMe env node = error("Implement addMe")





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
edgeBounce :: NodeEnvironment -> Signal -> IO Proceed

-- Phase 1: Left value, bounce on.
edgeBounce env (EdgeRequest origin (EdgeData dir (Left n))) = do

      let buildSignal = EdgeRequest origin . EdgeData dir
      atomically $ do
            writeTBQueue (_st1c env) $ case n of
                  0 -> buildSignal . Right $ 1 / _lambda config
                  k -> buildSignal . Left  $ k - 1
            toIO env $ printf "Bounced %s (%d left)" (show origin) n

      return Continue

-- Phase 2: either accept or bounce on with adjusted acceptance
-- probability.
--
-- (Note that bouncing on always decreases the denial probability, even in case
-- the reason was not enough room.)
edgeBounce env (EdgeRequest origin (EdgeData dir (Right p))) = do

      -- "Bounce on" action with denial probabillity decreased by lambda
      let buildSignal = EdgeRequest origin . EdgeData dir
          bounceOn = atomically $ writeTBQueue (_st1c env) $
                buildSignal . Right $ p / _lambda config

      -- Checks whether there's still room for another entry. The TVar can
      -- either be the set of known or "known by" nodes.
      let threshold = _maxNeighbours config
          isRoomIn tVar sizeF = atomically $
                (< threshold) . fromIntegral . sizeF <$> readTVar tVar

      -- Roll whether to accept the query first, then check whether there's
      -- room. In case of failure, bounce on.
      acceptEdge <- (> p) <$> randomRIO (0,1)
      case (acceptEdge, dir) of
            (False, _) -> bounceOn
            (True, Request) -> do
                  isRoom <- isRoomIn (_knownNodes env) Set.size
                  if isRoom then do -- TODO: Accept. Spawn client, send clientAdded message.
                                  errorCPP("Accept, spawn client, send ClientAdded")
                            else bounceOn
            (True, Announce) -> do
                  isRoom <- isRoomIn (_knownBy env) Map.size
                  if isRoom then do -- TODO: Accept. Send addMe approval.
                                  errorCPP("Accept, send AddMe")
                            else bounceOn

      return Continue

-- Bad signal received
edgeBounce env signal = do
      atomically $ toIO env $ printf ("Signal %s received by edgeBounce;"
                                   ++ "this should never happen") (show signal)
      return Continue