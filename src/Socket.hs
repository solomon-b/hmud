module Socket ( Handle(..)
              , Config(..)
              , withHandle
              , acceptHandle
              , readHandle
              , sendHandle) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Network.Socket.ByteString (recv, sendAll)
import Network.Socket hiding (recv)


----------------
---- Handle ----
----------------

newtype Handle = Handle { hSock :: Socket }

data Config = Config
    { cPort          :: Int
    , cSocketOptions :: [(SocketOption, Int)]
    , cConnections   :: Int
    , cAddrInfo      :: Maybe AddrInfo
    , cHostname      :: Maybe HostName
    , cServiceName   :: Maybe ServiceName
    }

newHandle :: Config -> IO Handle
newHandle config = do
    addrinfos <- getAddrInfo (cAddrInfo config) (cHostname config) (cServiceName config)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    mapM_ (uncurry $ setSocketOption sock) (cSocketOptions config)
    bind sock $ addrAddress serveraddr
    listen sock $ cConnections config
    return $ Handle sock

closeHandle :: Handle -> IO ()
closeHandle (Handle sock) = close sock

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config = bracket (newHandle config) closeHandle


-----------------
---- Actions ----
-----------------

acceptHandle :: Handle -> IO Handle
acceptHandle (Handle sock) = do
    (sock', _) <- accept sock
    return $ Handle sock'

readHandle :: Handle -> IO ByteString
readHandle (Handle sock) = recv sock 1024

sendHandle :: Handle -> ByteString -> IO ()
sendHandle (Handle sock) = sendAll sock
