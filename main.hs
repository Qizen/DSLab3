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

    forkIO $ connDispatchLoop sock chan
    
    mainLoop sock chan

-- mainLoop just listens for "KILL" on a channel to shut down the application
mainLoop :: Socket -> Chan Msg -> IO ()
mainLoop sock chan = do
    message <- readChan chan
    case message of 
        "KILL" -> killConn sock
        _ -> mainLoop sock chan

-- connDispatchLoop handles dispatching jobs to the thread pool
connDispatchLoop :: Socket -> Chan Msg -> IO ()
connDispatchLoop sock chan = do
    threadQueue <- atomically $ newTQueue
    threadPoolIO 4 runConn threadQueue
    forever $ do
        --accept a connection
        conn <- accept sock
        aChan <- dupChan chan
        atomically $ writeTQueue threadQueue (conn, aChan)

-- runConn processes messages received from a socket
runConn :: ((Socket, SockAddr), Chan Msg) -> IO ()
runConn ((sock, sockAddr), chan) = do
    rooms <- newEmptyMVar
    putMVar rooms []
    putStrLn "in Runn Conn"
    aChan <- dupChan chan
    
    -- spawn a thread which will send things on the chan to clients
    forkIO (forever $ do
                  line <- readChan aChan
                  m <- takeMVar rooms
                  --TODO if m contains the room from the line on the chan
                  putStrLn $ "line: " ++ line
                  send sock line
                  putMVar rooms m)
    connLoop ((sock, sockAddr), chan)

connLoop :: ((Socket, SockAddr), Chan Msg) -> IO ()
connLoop ((sock, sockAddr), chan) = do
     eitherMsg <- try $ recv sock 1024
     case eitherMsg of 
        Left e -> putStrLn $ "SOCKET ERR: " ++ show (e :: IOException)
        Right msg -> do
            let split = words msg
            case split of
                [] -> return ()
                x:xs -> case x of
                    "KILL_SERVICE" -> writeChan chan "KILL"
                    "HELO" -> processHelo (sock, sockAddr, msg)
                    "msg" ->writeChan chan $ "### " ++ unwords xs ++ "\n"
                    --TODO Change this to something sensible
                    _ -> parseCmd sock chan msg
                    --_ -> return () --process other messages here    
            connLoop ((sock, sockAddr), chan)

parseCmd :: Socket -> Chan Msg -> String -> IO ()
parseCmd sock chan cmd = do
    fullCmd <- recvWhile (\str -> any (==True) (map (not.isSpace) str)) cmd sock
    putStrLn fullCmd
    case (parseMsgString fullCmd) of
        Just (Join a) -> do 
                    let msg = createConfirmJoin (joinChatRmName a) (joinClientIp a) (show (joinPort a)) (joinClientName a)
                    send sock msg 
                    putStrLn "Join\n"
                    
        Just (Chat b) -> do 
                    putStrLn "Chat\n"
                    let str = createChatMsg (show (chatChatRmId b)) (chatClientName b) (chatMessage b)
                    --putStrLn ("ID: " ++ show (chatChatRmId b))
                    --putStrLn ("NM: " ++ (chatClientName b))
                    --putStrLn ("MS: " ++ (chatMessage b))
                    --putStrLn str
                    writeChan chan str    
        _ -> putStrLn "not join"

recvWhile :: (String -> Bool) -> String -> Socket -> IO String
recvWhile func str sock = do
    eitherMsg <- try $ recv sock 1024
    case eitherMsg of
        Left e-> return $ "SOCKETERROR" ++ show (e :: IOException) 
        Right msg -> if func msg then recvWhile func (str ++ msg) sock
                     else return str

-- killConn kills the service
killConn :: Socket -> IO ()
killConn sock = do
    putStrLn "Killing the Service"
    close sock
    exitSuccess

-- Process the HELO message
processHelo :: (Socket, SockAddr, String) -> IO ()
processHelo (sock, sockAddr, msg) = do
        if length split < 2 then return ()
        else do
            send sock $ "HELO " ++ split !! 1 ++ "\n\
                \IP:" ++ ip ++ "\n\
                \Port:" ++ port ++ "\n\
                \StudentID:12312907\n"
            return ()
    where split = words msg
          addr = splitOn ":" (show sockAddr)
          ip = addr !! 0
          port = addr !! 1

--creates a thread pool
threadPoolIO :: Int -> (a -> IO ()) -> TQueue (a) -> IO ()
threadPoolIO numWorkers func queue = forM_ [1..numWorkers] $ \i -> forkIO $ spawnWorkerIO i func queue

--spawns workers for a thread pool
spawnWorkerIO :: Int -> (a -> IO ()) -> TQueue (a) -> IO ()
spawnWorkerIO id func queue = forever $ do 
    putStrLn ("Thread " ++ show id ++ " running!")
    param <- atomically $ readTQueue queue
    func param
 
