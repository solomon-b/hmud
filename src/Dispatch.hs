{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Dispatch where

{-
Message Dispatching to the telnet client.
-}

import Control.Concurrent.Async
import Control.Concurrent.STM (TChan)
import Control.Monad (forever)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString as BS (pack)
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)

import Parser
import Socket
import TelnetLib
import Types 
    ( UserEnv(..)
    , HasSocketHandle(..)
    , MonadTChan(..)
    , MonadTCP(..)
    , Response(..)
    , tshow
    )

data ParseState = Normal | Multiline [()] deriving Show

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

cmdLoopInner :: 
    ( MonadTChan m
    , MonadTCP m
    , MonadIO m
    , MonadState ParseState m
    ) => Socket.Handle -> TChan Command -> m ()
cmdLoopInner handle cmdChan = forever $ do
    s         <- get
    rawMsg    <- readHandle' handle
    cmds <- runExceptT $ f s rawMsg
    liftIO $ print cmds
    case cmds of
        Right (cmd, s') -> do
            liftIO $ print cmd
            writeChannel cmdChan cmd
            put s'
        Left _ -> return () -- TODO: Log Error Here
    where f state rawMsg = do
            bs <- processStream rawMsg
            case state of
                Normal -> do
                    cmd <- runParse bs
                    case cmd of
                        Login -> return (Login, Multiline [()])
                        _     -> return (cmd,   Normal)
                Multiline cs -> do
                    str <- runWordParse rawMsg
                    if null cs 
                    then return (Word str, Normal)
                    else return (Word str, Multiline cs)

dispatchLoop ::
    forall m.
    ( MonadTChan m
    , MonadTCP m
    , MonadIO m
    ) => Socket.Handle -> TChan Command -> TChan Response -> TChan Response -> m ()
dispatchLoop handle cmdChan respChan publicChan = 
    let respLoop :: IO ()
        respLoop = forever $ do
            resp <- readChannel respChan
            sendHandle' handle . encodeUtf8 $ tshow resp
        --cmdLoop :: IO ()
        --cmdLoop = forever $ do
        --    rawMsg  <- readHandle' handle
        --    let command :: Either AppError Command
        --        command = do
        --            bs      <- processStream rawMsg
        --            runParse bs 
        --    case command of
        --        Left _ -> return () -- TODO: Log Here
        --        Right command' -> writeChannel cmdChan command'
        cmdLoop :: IO ()
        cmdLoop = liftIO . void $ execStateT (cmdLoopInner handle cmdChan) Normal
        publicLoop :: IO ()
        publicLoop = forever $ do
            public  <- readChannel publicChan
            sendHandle' handle . encodeUtf8 $ tshow public
    in liftIO $ race_ respLoop (race_ cmdLoop publicLoop)
