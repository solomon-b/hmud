{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM hiding (stateTVar)
--import Control.Exception (bracket)
import Control.Monad (forever, void)
import qualified Control.Monad.Reader as R
import Control.Monad.IO.Unlift
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.List (find)
import qualified Data.Map.Strict as M
import Database.SQLite.Simple (open)
import Network.Socket

import Dispatch
import Prompts
import SqliteLib (getUsersDb)
import Types 
    ( Env(..)
    , GameState(..)
    , MonadMessaging(..)
    , MonadGameState(..)
    , MonadTCP(..)
    , MonadPrompt(..)
    , MonadThread(..)
    , ThreadEnv(..)
    , User(..)
    )
import World


createSocket :: Integer -> IO Socket
createSocket port = do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                              Nothing (Just $ show port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock ReuseAddr 1 
    bind sock (addrAddress serveraddr)
    listen sock 1
    return sock

userLoop :: 
    ( R.MonadReader ThreadEnv m
    , MonadGameState m
    , MonadThread m
    , MonadUnliftIO m
    , MonadMessaging m
    , MonadPrompt m
    ) => m ()
userLoop = forever $ do
    state  <- readState'
    thread <- getThread
    readTChanLoop
    let user :: Maybe (User, ThreadId)
        user = find (\(_, tid) -> tid == thread) (globalActiveUsers state)

    case user of
        Just _  -> gamePrompt
        Nothing -> mainMenuPrompt

mainLoop :: 
    ( R.MonadReader Env m
    , R.MonadIO m
    , MonadGameState m
    , MonadUnliftIO m
    , MonadTCP m
    , MonadMessaging m
    ) => m ()
mainLoop = forever $ do
    (Env conn sock stateTVar wChannel users) <- R.ask
    (sock', _) <- acceptSocket sock
    rChannel   <- duplicateChannel wChannel
    userIdTvar <- liftIO . atomically $ newTVar Nothing

    drainTChanLoop rChannel
    
    void . liftIO $ do
        putStrLn "Got connection, handling query"
        let threadEnv = ThreadEnv conn sock' stateTVar wChannel rChannel userIdTvar users
        forkIO $ runReaderT userLoop threadEnv

main :: IO ()
main = withSocketsDo $ do
    conn <- open "hmud.db"
    gameSock <- createSocket 78
    state <- atomically $ newTVar (GameState M.empty world playerMap)
    wChannel <- newTChanIO
    users <- liftIO $ getUsersDb conn
    let env = Env conn gameSock state wChannel users
    runReaderT mainLoop env 
