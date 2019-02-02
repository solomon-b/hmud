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
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Reader
import Data.ByteString as BS (pack, append)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Network.Socket.ByteString (sendAll)

import Types (ThreadEnv(..)
             , HasSocket(..)
             , MonadMessaging(..)
             , MonadTCP(..)
             )


forkReader :: ReaderT r IO () -> ReaderT r IO ThreadId
forkReader action = do
    env <- ask
    liftIO . forkIO $ runReaderT action env

forkUnliftIO :: MonadUnliftIO m => m () -> m ThreadId
forkUnliftIO r = withRunInIO $ \run -> forkIO (run r)

suppressEcho ::
    ( R.MonadReader ThreadEnv m
    , MonadTCP m
    ) => m ()
suppressEcho = do
    sock <- R.asks getSocket
    --liftIO . print $ BS.append (encodeUtf8 "suppressing Echo: ") (BS.pack [255,251,1])
    sendSocket sock $ BS.pack [255,251,1]

unsuppressEcho ::
    ( R.MonadReader ThreadEnv m
    , MonadTCP m
    ) => m ()
unsuppressEcho = do
    sock <- R.asks getSocket
    --liftIO . print $ BS.pack [255,252,1]
    sendSocket sock $ BS.pack [255,252,1]

sendMsg' :: 
    ( R.MonadReader ThreadEnv m
    , R.MonadIO m
    ) => Text -> m ()
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

readTChanLoop :: 
    ( R.MonadReader ThreadEnv m
    , MonadUnliftIO m
    , MonadMessaging m
    ) => m ()
readTChanLoop = void . forkUnliftIO . forever $ do
    rChannel <- R.asks threadEnvRChannel
    msg <- readChannel rChannel
    sendMsg' msg

drainTChanLoop :: (R.MonadIO m, MonadUnliftIO m) => TChan a -> m ()
drainTChanLoop rChannel = 
    void . forkUnliftIO . forever . liftIO . atomically $ readTChan rChannel
