import WebLogic.GameOperations
import WebLogic.TypesSerializer
import Control.Monad (unless,when)
import Data.String.Conversions
import qualified Data.ByteString as S
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv)
import Logic.Types
import Data.Binary

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
    rMsg <- recv sock 300
    let gs = dserializeGameState $ convertString rMsg
    print (gs :: GameState)

    unless (S.null rMsg) $ do
        mainLoop sock
