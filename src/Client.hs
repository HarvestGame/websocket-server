import WebLogic.TypeSerializer
import WebLogic.GameOperations
import Control.Monad (unless,when)
import qualified Data.ByteString as S
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv)


main :: IO ()
main = withSocketsDo $ do
    addrInfo <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4242")
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect sock (addrAddress serverAddr)
    mainLoop sock
    sClose sock
    putStrLn "Disconnected!"

mainLoop :: Socket -> IO ()
mainLoop sock = do
  rMsg <- recv sock 1024
  putStr $ deserializeGameState rMsg
  unless (S.null rMsg) $ do
    mainLoop sock
