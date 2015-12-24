import WebLogic.GameOperations
import WebLogic.TypesSerializer
import Logic.Types
import Network.Socket
import Data.String.Conversions

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
    putStrLn "Start!"
    mainLoop sock 1 gs
    sClose sock

mainLoop :: Socket -> Integer -> GameState -> IO ()
mainLoop sock num gs = do
    -- accept one connection and handle it
    let clientNo = num
    conn <- accept sock
    runConn conn clientNo gs
    mainLoop sock (clientNo + 1) (updateTick gs)

runConn :: (Socket, SockAddr) -> Integer -> GameState -> IO ()
runConn (sock, sockAddr) clientNo gs = do
    putStrLn $ "Got connection " ++ show sockAddr
    print gs
    let msg =  convertString (serializeGameState gs) ++ "\n"
    putStrLn msg
    putStrLn $ show $ length msg
    send sock msg
    print "send"
