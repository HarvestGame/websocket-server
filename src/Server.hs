import WebLogic.GameOperations
import WebLogic.TypeSerializer
import Network.Socket


main :: IO ()
main = do
    let gs = getInitialGameState 2 -- Num of players
    -- create socket
    sock <- socket AF_INET Stream 0
    -- make socket immediately reusable - eases debugging.
    setSocketOption sock ReuseAddr 1
    -- listen on TCP port 4242
    bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    -- allow a maximum of 2 outstanding connections
    listen sock 2
    mainLoop sock 1 gs

mainLoop :: Socket -> Integer -> GameState -> IO ()
mainLoop sock num = do
    -- accept one connection and handle it
    let clientNo = num
    conn <- accept sock
    runConn conn clientNo gs
    gs = updateTick gs
    mainLoop sock $ clientNo + 1 $ gs

runConn :: (Socket, SockAddr) -> Integer -> GameState -> IO ()
runConn (sock, sockAddr) clientNo = do
    putStrLn $ "Got connection " ++ show sockAddr
    --send sock $ "Hi! You are my client: " ++ (show clientNo) ++ "\n"
    send sock $ serializeGameState gs ++ "\n"
    sClose sock
