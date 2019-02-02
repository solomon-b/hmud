{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Dispatch where

{-
Message Dispatching to the telnet client.
-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever, void)
import qualified Control.Monad.Reader as R
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.ByteString as BS (pack, append)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Network.Socket.ByteString (sendAll)

import Types (ThreadEnv(..))


forkReader :: ReaderT r IO () -> ReaderT r IO ThreadId
forkReader action = do
    env <- ask
    liftIO . forkIO $ runReaderT action env

suppressEcho :: ReaderT ThreadEnv IO ()
suppressEcho = do
    sock <- asks threadEnvSock
    liftIO . print $ BS.append (encodeUtf8 "suppressing Echo: ") (BS.pack [255,251,1])
    liftIO . sendAll sock $ BS.pack [255,251,1]

unsuppressEcho :: ReaderT ThreadEnv IO ()
unsuppressEcho = do
    sock <- asks threadEnvSock
    liftIO . print $ BS.pack [255,252,1]
    liftIO . sendAll sock $ BS.pack [255,252,1]

sendMsg' :: (R.MonadReader ThreadEnv m, R.MonadIO m) => Text -> m ()
sendMsg' msg = do
    sock <- R.asks threadEnvSock
    liftIO . sendAll sock . encodeUtf8 $ T.append msg "\r\n"

sendMsg :: Text -> ReaderT ThreadEnv IO ()
sendMsg msg = do
    sock <- asks threadEnvSock
    liftIO . sendAll sock . encodeUtf8 $ T.append msg "\r\n"

broadcast :: Text -> ReaderT ThreadEnv IO ()
broadcast msg = do
    wChannel <- asks threadEnvWChannel
    liftIO . atomically $ writeTChan wChannel msg

readTChanLoop :: ReaderT ThreadEnv IO ()
readTChanLoop = void . forkReader . forever $ do
    rChannel <- asks threadEnvRChannel
    msg <- liftIO . atomically $ readTChan rChannel
    sendMsg msg

drainTChanLoop :: TChan a -> ReaderT r IO ()
drainTChanLoop rChannel =
    void . forkReader . forever . liftIO . atomically $ readTChan rChannel 
