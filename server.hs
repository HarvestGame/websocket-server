import Network.Socket

main :: IO ()
main = do
    -- create socket
    sock <- socket AF_INET Stream 0
    -- make socket immediately reusable - eases debugging.
    setSocketOption sock ReuseAddr 1
    -- listen on TCP port 4242
    bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    -- allow a maximum of 2 outstanding connections
    listen sock 2
    mainLoop sock 1

mainLoop :: Socket -> Integer -> IO ()
mainLoop sock num = do
    -- accept one connection and handle it
    let clientNo = num
    conn <- accept sock
    runConn conn clientNo
    mainLoop sock $ clientNo + 1

runConn :: (Socket, SockAddr) -> Integer -> IO ()
runConn (sock, sockAddr) clientNo = do
    putStrLn $ "Got connection " ++ show sockAddr
    send sock $ "Hi! You are my client: " ++ (show clientNo) ++ "\n"
    sClose sock
