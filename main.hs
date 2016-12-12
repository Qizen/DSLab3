module Main where

import Network.Socket
import Data.List.Split
import Data.Char
import System.Exit
import System.Environment
import Control.Concurrent
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Protocol

type Msg = String

data ServerState = ServerState { servSocket     :: Socket
                              -- , sockAddr       :: SockAddr
                               , servChan           :: Chan Msg
                               , roomNames      :: TVar [(String, Int)]
                               , roomClients    :: TVar [(Int, [Int])]
                               , clientSockets  :: TVar [(Int, Socket)] -- TODO these should all be maps...
                               , numClients     :: TVar Int
                               , nextIdNum    :: TVar Int
                               , nextRmId       :: TVar Int
                               }

main :: IO ()
main = do
    --read port number from command line
    args <- getArgs
    let port = args !! 0

    --create a TCP Socket
    sock <- socket AF_INET Stream 0

    setSocketOption sock ReuseAddr 1

    --bind the socket to port 
    bind sock (SockAddrInet (fromIntegral(read(port))) iNADDR_ANY) 

    --listen for at most 2 queued connections
    listen sock 2

    --create a new FIFO channel for threads to communicate
    chan <- newChan
    
    -- initialise the server's state
    roomNameList <- atomically $ newTVar []
    roomClientList <- atomically $ newTVar []
    cliSockets <- atomically $ newTVar []
    numClis <- atomically $ newTVar 0
    nextId <- atomically $ newTVar 0
    nxRmId <- atomically $ newTVar 0

    let serverState = ServerState {servSocket = sock, servChan = chan, roomNames = roomNameList, roomClients = roomClientList, clientSockets = cliSockets, numClients = numClis, nextIdNum = nextId, nextRmId = nxRmId}

    forkIO $ connDispatchLoop serverState
    
    mainLoop serverState

-- mainLoop just listens for "KILL" on a channel to shut down the application
mainLoop :: ServerState -> IO ()
mainLoop serv@ServerState{servSocket=sock, servChan = chan} = do
    message <- readChan chan
    case message of 
        "KILL" -> killConn sock
        _ -> mainLoop serv

-- connDispatchLoop handles dispatching jobs to the thread pool
connDispatchLoop :: ServerState -> IO ()
connDispatchLoop serv@ServerState{servSocket = sock, servChan = chan, numClients = numClis, clientSockets = cliSocks} = do
    threadQueue <- atomically $ newTQueue
    threadPoolIO 4 runConn threadQueue
    forever $ do
        --accept a connection
        conn@(cliSock, cliSockAddr) <- accept sock
        
        aChan <- dupChan chan
        atomically $ writeTQueue threadQueue (serv, cliSock)

-- runConn processes messages received from a socket
runConn :: (ServerState, Socket) -> IO ()
runConn  (serv@ServerState{servSocket=sock, servChan = chan, numClients = numClis, clientSockets = cliSocks, nextIdNum = nextId}, cliSock) = do
    --add a new client
    atomically $ modifyTVar' numClis (+1)
    --atomically assign clientId
    clientId <- atomically $ ( do 
                                lst <- readTVar nextId
                                swapTVar nextId (lst + 1) )
    putStrLn $ "client number " ++ show clientId ++ " assigned" 
    -- add to the list of clients
    atomically $ modifyTVar' cliSocks ( (clientId, cliSock) :)

    putStrLn "in Runn Conn"
    aChan <- dupChan chan
    
    -- spawn a thread which will send things on the chan to clients
    forkIO (forever $ do
                  line <- readChan aChan
                  --TODO if m contains the room from the line on the chan
                  putStrLn $ "line: " ++ line
                  send cliSock line -- TODO be wary of exactly what sock this is
                  )
    connLoop serv cliSock clientId

connLoop :: ServerState -> Socket -> Int -> IO ()
connLoop serv@ServerState{servSocket=sock, servChan = chan} cliSock cliId= do
     eitherMsg <- try $ recv cliSock 1024
     case eitherMsg of 
        Left e -> putStrLn $ "SOCKET ERR: " ++ show (e :: IOException) -- should decrement numClis here
        Right msg -> do
            let split = words msg
            case split of
                [] -> return ()
                x:xs -> case x of
                    "KILL_SERVICE" -> writeChan chan "KILL"
                    "HELO" -> processHelo (cliSock, msg)
                    "msg" ->writeChan chan $ "### " ++ unwords xs ++ "\n"
                    --TODO Change this to something sensible
                    _ -> parseCmd serv cliSock cliId msg
                    --_ -> return () --process other messages here    
            connLoop serv cliSock cliId

parseCmd :: ServerState -> Socket -> Int -> String -> IO ()
parseCmd serv@ServerState{servSocket = sock, servChan = chan} cliSock cliId cmd = do
    --fullCmd <- recvWhile (\str -> any (==True) (map (not.isSpace) str)) cmd cliSock
    let fullCmd = cmd    
    putStrLn fullCmd
    case (parseMsgString fullCmd) of
        Just (Join a) -> do 
                    handleJoin serv cliSock cliId a
                    
        Just (Chat b) -> do 
                    putStrLn "Chat\n"
                    let str = createChatMsg (show (chatChatRmId b)) (chatClientName b) (chatMessage b)
                    sendToRoom serv (chatChatRmId b) str    
        _ -> putStrLn "not join"

recvWhile :: (String -> Bool) -> String -> Socket -> IO String
recvWhile func str sock = do
    eitherMsg <- try $ recv sock 1024
    case eitherMsg of
        Left e-> return $ "SOCKETERROR" ++ show (e :: IOException) 
        Right msg -> if func msg then recvWhile func (str ++ msg) sock
                     else return str

-- Handle the join message
handleJoin :: ServerState -> Socket -> Int -> RequestJoin -> IO ()
handleJoin serv@ServerState{roomNames = rmNames, nextRmId = nextRm, roomClients = rmClis, clientSockets = cliSocks} cliSock cliId joinMsg = do
    --getOrCreateChatRm    
    rms <- atomically $ readTVar rmNames
    putStrLn $ "Rms " ++ show rms
    putStrLn $ jRmName ++ "\n"
    let ourRm = filter ((==jRmName) . fst) rms
    putStrLn $ (show ourRm) ++ "\n"
    let rmId = (case ourRm of [] -> atomically $ (do    lst <- readTVar nextRm
                                                        modifyTVar' rmClis (((lst),[cliId]):)
                                                        modifyTVar' rmNames ((jRmName, lst):)
                                                        swapTVar nextRm (lst+1))
                              _ -> return $ snd (ourRm!!0))
    
    -- get the Int from the IO Int
    unwrappedRmId <- rmId
    
    --addClientToRoom
    atomically $ do modifyTVar' rmClis (map (\a->if fst a == unwrappedRmId then ((fst a), cliId:(snd a)) else a))
    putStrLn $ "added " ++ (show cliId) ++ "\n"

    let msg = createConfirmJoin (jRmName) (jIp) (show (jPort)) (show unwrappedRmId) (jName)
    send cliSock msg 
    let joinString = jName ++ " has joined " ++ jRmName ++ "\n"     
    sendToRoom serv unwrappedRmId joinString
    putStrLn "Join\n"
    where   jRmName = joinChatRmName joinMsg
            jIp = joinClientIp joinMsg
            jPort = joinPort joinMsg
            jName = joinClientName joinMsg

sendToRoom :: ServerState -> Int -> String -> IO ()
sendToRoom serv@ServerState{roomClients = rmClis, clientSockets = cliSocks} roomId msg = do
    rmsToClis <- atomically $ readTVar rmClis
    putStrLn $ (show rmsToClis) ++ "\n"
    clisToSocks <- atomically $ readTVar cliSocks
    let ourClisArr = map snd ( filter ((==roomId).fst) rmsToClis)
    let ourClis = (if (length ourClisArr) > 0 then ourClisArr!!0 else [])
    let memberSocks = map snd (filter (\a-> if any (==(fst a)) ourClis then True else False) clisToSocks)
    mapM ((flip send) msg) memberSocks
    return ()

-- Return a chat room ID, creating one if necessary
getOrCreateChatRmId :: String -> TVar [(String, Int)] -> TVar Int -> STM Int
getOrCreateChatRmId name rooms nextRm = do 
    rms <- readTVar rooms
    let ourRm = filter ((==name) . fst) rms
    if ourRm == []
        then do newRmId <- (do
                                    lst <- readTVar nextRm
                                    swapTVar nextRm (lst+1) )                            
                modifyTVar rooms ((name, newRmId) :)
                return newRmId
        else return $ snd (ourRm !! 0)

-- killConn kills the service
killConn :: Socket -> IO ()
killConn sock = do
    putStrLn "Killing the Service"
    close sock
    exitSuccess

-- Process the HELO message
processHelo :: (Socket, String) -> IO ()
processHelo (sock, msg) = do
        ourSockAddr <- getSocketName sock
        port <- socketPort sock
        (Just ourAddr, _) <- getNameInfo [NI_NUMERICHOST] True False ourSockAddr 
        if length split < 2 then return ()
        else do
            send sock $ "HELO " ++ split !! 1 ++ "\n\
                \IP:" ++ ourAddr ++ "\n\
                \Port:" ++ (show port) ++ "\n\
                \StudentID:12312907\n"
            return ()
    where split = words msg

--creates a thread pool
threadPoolIO :: Int -> (a -> IO ()) -> TQueue (a) -> IO ()
threadPoolIO numWorkers func queue = forM_ [1..numWorkers] $ \i -> forkIO $ spawnWorkerIO i func queue

--spawns workers for a thread pool
spawnWorkerIO :: Int -> (a -> IO ()) -> TQueue (a) -> IO ()
spawnWorkerIO id func queue = forever $ do 
    putStrLn ("Thread " ++ show id ++ " running!")
    param <- atomically $ readTQueue queue
    func param
 
