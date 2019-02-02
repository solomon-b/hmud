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
import Types ( Env(..)
             , GlobalState(..)
             , MonadMessaging(..)
             , MonadState(..)
             , MonadTCP(..)
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

forkUnliftIO :: MonadUnliftIO m => m () -> m ThreadId
forkUnliftIO r = withRunInIO $ \run -> forkIO (run r)

drainTChanLoop' :: (R.MonadIO m, MonadUnliftIO m) => TChan a -> m ()
drainTChanLoop' rChannel = 
    void . forkUnliftIO . forever . liftIO . atomically $ readTChan rChannel

readTChanLoop' :: ( R.MonadReader ThreadEnv m
                  , MonadUnliftIO m
                  , MonadMessaging m
                  ) => m ()
readTChanLoop' = void . forkUnliftIO . forever $ do
    rChannel <- R.asks threadEnvRChannel
    msg <- readChannel rChannel
    sendMsg' msg

userLoop :: ( R.MonadReader ThreadEnv m
             , MonadState m
             , MonadThread m
             , MonadUnliftIO m
             , MonadMessaging m
             ) => m ()
userLoop = forever $ do
    env <- R.ask
    state  <- readState'
    thread <- getThread
    readTChanLoop'
    let user :: Maybe (User, ThreadId)
        user = find (\(_, tid) -> tid == thread) (globalActiveUsers state)

    case user of
        Just _ -> liftIO $ runReaderT gamePrompt env
        Nothing -> liftIO $ runReaderT mainMenuPrompt env


mainLoop :: ( R.MonadReader Env m
            , R.MonadIO m
            , MonadState m
            , MonadUnliftIO m
            , MonadTCP m
            , MonadMessaging m
            ) => m ()
mainLoop = forever $ do
    (Env conn sock stateTVar wChannel users) <- R.ask
    (sock', _) <- acceptSocket sock
    rChannel   <- duplicateChannel wChannel
    userIdTvar <- liftIO . atomically $ newTVar Nothing

    drainTChanLoop' rChannel
    
    void . liftIO $ do
        putStrLn "Got connection, handling query"
        let threadEnv = ThreadEnv conn sock' stateTVar wChannel rChannel userIdTvar users
        forkIO $ runReaderT userLoop threadEnv

main :: IO ()
main = withSocketsDo $ do
    conn <- open "hmud.db"
    gameSock <- createSocket 78
    state <- atomically $ newTVar (GlobalState M.empty world playerMap)
    wChannel <- newTChanIO
    users <- liftIO $ getUsersDb conn
    let env = Env conn gameSock state wChannel users
    runReaderT mainLoop env 
