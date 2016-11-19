module Main where

import Network.Socket
import Data.List.Split
import System.Exit
import System.Environment
import Control.Concurrent
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception

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

-- conDispatchLoop handles dispatching jobs to the thread pool
connDispatchLoop :: Socket -> Chan Msg -> IO ()
connDispatchLoop sock chan = do
    threadQueue <- atomically $ newTQueue
    threadPoolIO 4 runConn threadQueue
    forever $ do
        --accept a connection
        conn <- accept sock
        atomically $ writeTQueue threadQueue (conn, chan)

-- runConn processes messages received from a socket
runConn :: ((Socket, SockAddr), Chan Msg) -> IO ()
runConn ((sock, sockAddr), chan) = do
    putStrLn "in Runn Conn"
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
                    _ -> return () --process other messages here    
            runConn ((sock, sockAddr), chan)

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
 
