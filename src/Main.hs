{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM hiding (stateTVar)
import Control.Monad (forever)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.IO.Unlift
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import Network.Socket

import Dispatch
import Prompts
import qualified Socket
import qualified SqliteLib as SQL
import Types 
    ( Env(..)
    , GameState(..)
    , MonadTChan(..)
    , MonadGameState(..)
    , MonadTCP(..)
    , MonadPlayer(..)
    , MonadPrompt(..)
    , MonadThread(..)
    , UserEnv(..)
    , Response(..)
    )
import World

newtype AppM env a = App { unAppM :: ReaderT env IO a}
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadUnliftIO
             , MonadReader env
             , MonadGameState
             , MonadPlayer
             , MonadPrompt
             , MonadTChan
             , MonadTCP
             , MonadThread
             )

userLoop ::
    ( MonadReader UserEnv m
    , MonadGameState m
    , MonadThread m
    , MonadUnliftIO m
    , MonadTChan m
    , MonadPlayer m
    , MonadPrompt m
    , MonadTCP m
    ) => m ()
userLoop = forever $ do
    eUser     <- getUser
    pubTChan  <- asks userEnvPubTChan
    respTChan <- asks userEnvRespTchan
    case eUser of
        -- User is logged in:
        Right _ -> do
            response <- gamePrompt
            case response of
                Right (resp@(RespSay _ _)) -> writeChannel pubTChan resp
                Right resp -> writeChannel respTChan resp
                Left  e    -> writeChannel respTChan RespBadCommand >> liftIO (print e)
        -- User is not logged in:
        Left _ -> do
            resp <- mainMenuPrompt
            case resp of
                Right resp' -> writeChannel respTChan resp'
                Left  e     -> writeChannel respTChan RespBadCommand >> liftIO (print e)

mainLoop :: AppM Env ()
mainLoop = forever $ do
    (Env stateTVar pubTChan conn sock) <- ask
    sock'     <- acceptHandle' sock
    cmdTChan  <- createTChan
    respTChan <- createTChan
    pubTChan' <- duplicateChannel pubTChan
    uidTvar   <- liftIO . atomically $ newTVar Nothing
    let userEnv = UserEnv conn sock' stateTVar pubTChan' cmdTChan respTChan uidTvar
    liftIO . forkIO $ race_ (runAppM userEnv userLoop) 
                            (dispatchLoop sock' cmdTChan respTChan pubTChan')

runAppM :: env -> AppM env a -> IO a
runAppM env = flip runReaderT env . unAppM

socketConfig :: Socket.Config
socketConfig = 
    Socket.Config 
    { Socket.cPort          = 78
    , Socket.cSocketOptions = [(ReuseAddr, 1)]
    , Socket.cConnections   = 1
    , Socket.cAddrInfo      = Just (defaultHints {addrFlags = [AI_PASSIVE]})
    , Socket.cHostname      = Nothing
    , Socket.cServiceName   = Just "78"
    }

dBConfig :: SQL.Config
dBConfig = SQL.Config "hmud.db"

launchApp :: SQL.Config -> Socket.Config -> (SQL.Handle -> Socket.Handle -> Env) -> IO ()
launchApp sqlC sockC env =
    SQL.withHandle sqlC (\db -> 
        Socket.withHandle sockC (\sock ->
            runAppM (env db sock) mainLoop))

main :: IO ()
main = withSocketsDo $ do
    state    <- atomically $ newTVar (GameState M.empty world playerMap)
    wChannel <- newTChanIO
    let env = Env state wChannel --users
    launchApp dBConfig socketConfig env
