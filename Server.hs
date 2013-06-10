module Server (
      randomSocket,
      bootstrap,
      serverLoop
) where




-- Algorithm idea: Create a special Bootstrap signal. When receiving such a
-- signal, the receiving node sends out N Request/Announce signals, with the
-- origin set to the issuing node.
--
-- To combat abuse, the client should hold a timestamped version of the command,
-- and only accept a new one after X seconds.
bootstrap :: PortNumber -> IO HostName
bootstrap port = do

      -- Send out signal to the bootstrap node
      -- TODO: Add a function to find a random bootstrap node
      let bNode = error("Bootstrap node search")
          bHost = _host bNode
          bPort = PortNumber $ _port bNode
      bracket (connectTo bHost bPort) hClose $ \h -> do
            send h (Bootstrap port)
            (YourHostIs host) <- receive h
            -- TODO: Handle timeouts, yell if pattern mismatch
            return host

      -- TODO: Handle failing bootstrap by retrying, i.e. catch -> recurse




-- | Tries opening a socket on a certain amount of random ports.
randomSocket :: Word -> IO Socket
randomSocket 0 = error("Couldn't find free port")
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
worker :: (Handle, HostName, PortNumber)
       -> NodeEnvironment
       -> IO ()
worker (h, host, port) env = untilTerminate $ do

      -- TODO: Ignore signals sent by nodes not registered as upstream.
      --       Open issues:
      --         - Do this here or in the server loop?
      --         - How do the ignored nodes find out they're being ignored?
      --         - Nodes trying to connect to bootstrap over the current node
      --           should not be ignored. Make a special Bootstrap signal that
      --           is only sent in the very beginning?

      -- Update "last heard of" timestamp. Note that this will not add a valid
      -- return address (but some address the incoming connection happens to
      -- have)!
      let fromNode = Node { _host = host, _port = port }
      makeTimestamp >>= atomically . updateTimestamp env fromNode

      -- TODO: Error handling: What to do if rubbish data comes in?
      signal <- receive h

      case signal of
            Message {}         -> floodMessage env signal
            ShuttingDown node  -> shuttingDown env node
            IAddedYou node     -> iAddedYou env node
            AddMe              -> error("Implement addMe handling")
            EdgeRequest {}     -> edgeBounce env signal
            KeepAlive          -> return Continue -- Just update timestamp
            Bootstrap bPort    -> helpBootstrap env h host bPort
            YourHostIs {}      -> yourHostIsError env
            NotYourNeighbour   -> error("Implement NotYourNeighbour handling")













-- | A node should never receive a YourHostIs signal unless it issued
--   Bootstrap. In case it gets one anyway, this function is called.
yourHostIsError :: NodeEnvironment -> IO Proceed
yourHostIsError env = do
      atomically . toIO env . print $
            "YourHostIs signal received without bootstrap process. This is a bug."
      return Terminate





-- | Special connection handler used only as response to a Bootstrap signal.
--   Will respond with the issuer's hostname, and send out EdgeRequests in its
--   name.
--
--   Note that bootstrapping will not add the contacted node to a pool, all it
--   does is pass on signals.
helpBootstrap :: NodeEnvironment
              -> Handle
              -> HostName
              -> PortNumber
              -> IO Proceed
helpBootstrap env h host port = do

      -- Respond with hostname
      send h (YourHostIs host) `finally` hClose h

      -- Send out n EdgeRequests
      let n = _minNeighbours config
          node = Node { _host = host, _port = port }
      forM_ [1..n] $ \_ -> sendEdgeRequest env node Request
      forM_ [1..n] $ \_ -> sendEdgeRequest env node Announce

      return Terminate






-- | Sends a message to the printer thread
floodMessage :: NodeEnvironment
             -> Signal
             -> IO Proceed
floodMessage env signal = do

      -- Only process the message if it hasn't been processed already
      process <- atomically $
            Set.member signal <$> readTVar (_handledQueries env)

      when process $ atomically $ do

            -- Add signal to the list of already handled ones
            modifyTVar (_handledQueries env) (Set.insert signal)

            -- Print message on the current node
            let ~(Message _timestamp message) = signal
            toIO env $ putStrLn message

            -- Propagate message on to all clients
            writeTChan (_stc env) signal

      return Continue -- Keep the connection alive, e.g. to get more messages
                      -- from that node.





-- | Updates the timestamp in the "last heard of" database (if present).
updateTimestamp :: NodeEnvironment
                -> Node
                -> Timestamp
                -> STM ()
updateTimestamp env node timestamp = modifyTVar (_knownBy env) $
      Map.adjust (const timestamp) node





-- | When received, remove the issuing node from the database to ease network
--   cleanup.
shuttingDown :: NodeEnvironment
             -> Node
             -> IO Proceed
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
--   This should be the first signal the current node receives from another node
--   choosing it as its new neighbour.
iAddedYou :: NodeEnvironment
          -> Node
          -> IO Proceed
iAddedYou env node = do
      timestamp <- makeTimestamp
      atomically $ do
            modifyTVar (_knownBy env) (Map.insert node timestamp)
            toIO env $ putStrLn $ "New upstream neighbour: " ++ show node
      return Continue -- Let's not close the door in front of our new friend :-)





-- | A node signals that it's ready to have another upstream neighbour added,
--   and gives the current node the permission to do so.
addMe :: NodeEnvironment
      -> Node
      -> IO Proceed
addMe = error("Implement addMe")





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
edgeBounce :: NodeEnvironment
           -> Signal
           -> IO Proceed

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