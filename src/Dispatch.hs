{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Dispatch where

{-
Message Dispatching to the telnet client.
-}

import Control.Concurrent.Async
import Control.Concurrent.STM (TChan)
import Control.Monad (forever)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (MonadState, execStateT, put, get)
import Data.ByteString as BS (pack)
import Data.Text.Encoding (encodeUtf8)

import Errors
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

-- Always define the naturals:
data Peano = Zero | Succ Peano deriving Show
data ParseState = Normal | Multiline Peano deriving Show

isZero :: Peano -> Bool
isZero Zero = True
isZero _    = False

decr :: Peano -> Peano
decr Zero = Zero
decr (Succ p) = p

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
    ) => Socket.Handle -> TChan (Either AppError Command) -> m ()
cmdLoopInner handle cmdChan = forever $ do
    s      <- get
    rawMsg <- readHandle' handle
    cmds   <- runExceptT $ parseRawMsg s rawMsg
    case cmds of
        Right (cmd, s') -> writeChannel cmdChan (Right cmd) >> put s'
        Left e -> writeChannel cmdChan (Left e)
    where parseRawMsg state rawMsg = do
            bs <- processStream rawMsg
            case state of
                Normal -> do
                    cmd <- runParse bs
                    case cmd of
                        Login -> return (Login, Multiline (Succ Zero))
                        _     -> return (cmd,   Normal)
                Multiline cs -> do
                    str <- runWordParse rawMsg
                    if isZero cs
                    then return (Word str, Normal)
                    else return (Word str, Multiline $ decr cs)

dispatchLoop ::
    forall m.
    ( MonadTChan m
    , MonadTCP m
    , MonadIO m
    ) => Socket.Handle -> TChan (Either AppError Command) -> TChan Response -> TChan Response -> m ()
dispatchLoop handle cmdChan respChan publicChan = 
    let respLoop :: IO ()
        respLoop = forever $ do
            resp <- readChannel respChan
            sendHandle' handle . encodeUtf8 $ tshow resp
        cmdLoop :: IO ()
        cmdLoop = liftIO . void $ execStateT (cmdLoopInner handle cmdChan) Normal
        publicLoop :: IO ()
        publicLoop = forever $ do
            public  <- readChannel publicChan
            sendHandle' handle . encodeUtf8 $ tshow public
    in liftIO $ race_ respLoop (race_ cmdLoop publicLoop)
