import WebLogic.GameOperations
import WebLogic.TypesSerializer
import Logic.Types
import Network.Socket
import Data.String.Conversions

import Control.Monad
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TVar

data ServerState = ServerState {
    clientNum :: Int,
    gameState :: GameState  
} deriving (Show)

main :: IO ()
main = do
    putStrLn "Start!"

    -- Create initial serverState
    sref <- newTVarIO $ ServerState 0 (getInitialGameState 2) -- Num of players
    -- create socket
    sock <- createMainSocket

    -- spawn the threads that will handle the connections and updates
    _ <- forkIO $ forever $ networkingLoop sock sref
    _ <- forever $ gameLoop sref

    return ()

-- | This is the main thread that updates the game
gameLoop :: TVar ServerState -> IO ()
gameLoop sref = do
    s <- atomically $ do
        modifyTVar sref $ \s -> s { gameState = updateTick $ gameState s }
        readTVar sref
    print s
    threadDelay $ 1000 * 1000

-- | Creates main endpoint to be used for incoming connections
createMainSocket :: IO Socket 
createMainSocket = do
    sock <- socket AF_INET Stream 0
    -- make socket immediately reusable - eases debugging.
    setSocketOption sock ReuseAddr 1
    -- listen on TCP port 4242
    bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    -- allow a maximum of 2 concurrent connections
    listen sock 2
    return sock

-- | This loops watches incoming connections and spawns client fibres
networkingLoop :: Socket -> TVar ServerState -> IO ()
networkingLoop sock sref = do
    -- accept one connection
    let clientNo = 0
    conn <- accept sock
    -- spawn a new thread for the client
    _ <- forkIO $ runConn conn 0 sref
    return ()

runConn :: (Socket, SockAddr) -> Int -> TVar ServerState -> IO ()
runConn (sock, sockAddr) clientNo sref = do
    putStrLn $ "Got connection " ++ show sockAddr
    forever $ do
        gs <- gameState <$> readTVarIO sref
        let msg = convertString (serializeGameState gs) ++ "\n"
        print "sending"
        send sock msg

        threadDelay $ 1000 * 1000
        
