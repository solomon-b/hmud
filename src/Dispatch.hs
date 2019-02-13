{-# LANGUAGE FlexibleContexts #-}
module Dispatch where

{-
Message Dispatching to the telnet client.
-}

import Control.Concurrent.Async
import Control.Concurrent.STM (TChan)
import Control.Monad (forever)
import Control.Monad.Reader
import Data.ByteString as BS (pack)
import Data.Text.Encoding (encodeUtf8)

import Socket
import Types 
    ( UserEnv(..)
    , HasSocketHandle(..)
    , MonadTChan(..)
    , MonadTCP(..)
    , Response(..)
    , tshow
    )

suppressEcho ::
    ( MonadReader UserEnv m
    , MonadTCP m
    ) => m ()
suppressEcho = do
    sock <- asks getSocketHandle
    sendHandle' sock $ BS.pack [255,251,1]

unsuppressEcho ::
    ( MonadReader UserEnv m
    , MonadTCP m
    ) => m ()
unsuppressEcho = do
    sock <- asks getSocketHandle
    sendHandle' sock $ BS.pack [255,252,1]

dispatchLoop ::
    ( MonadTChan m
    , MonadTCP m
    , MonadIO m
    ) => Socket.Handle -> TChan Response -> TChan Response -> m ()
dispatchLoop sock private public = 
    let privateLoop = forever $ do
            private' <- readChannel private
            sendHandle' sock . encodeUtf8 $ tshow private'
        publicLoop = forever $ do
            public'  <- readChannel public
            sendHandle' sock . encodeUtf8 $ tshow public'
    in liftIO $ race_ privateLoop publicLoop
