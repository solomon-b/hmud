{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import Control.Applicative
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

import HMud.Dispatch
import HMud.Prompts
import qualified HMud.Socket as Socket
import qualified HMud.SqliteLib as SQL
import HMud.Types
import HMud.Types.Classes
import HMud.World

newtype AppM env a = App { unAppM :: ReaderT env IO a}
  deriving ( Functor
           , Applicative
           , Alternative
           , Monad
           , MonadIO
           , MonadUnliftIO
           , MonadDB
           , MonadReader env
           , MonadGameState
           , MonadTChan
           , MonadTCP
           , MonadThread
           )

userLoop ::
  ( MonadReader UserEnv m
  , MonadIO m
  , MonadTChan m
  ) => m ()
userLoop = forever $ do
  env <- ask
  activeSession <- do
    res <- runExceptT $ runReaderT isActiveSession env
    case res of
      Left _ -> pure False
      Right res' -> pure res'

  pubTChan  <- asks userEnvPubTChan
  respTChan <- asks userEnvRespTchan
  if activeSession
    then do
      response <- runExceptT $ runReaderT gamePrompt env
      case response of
        Right (resp@(RespSay _ _)) -> writeChannel pubTChan resp
        Right resp -> writeChannel respTChan resp
        Left  err  -> writeChannel respTChan (RespAppError err) >> liftIO (print err)
    else do
      response <- runExceptT $ runReaderT mainMenuPrompt env
      case response of
        Right resp' -> writeChannel respTChan resp'
        Left  err   -> writeChannel respTChan (RespAppError err) >> liftIO (print err)

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
  { Socket.cPort          = 7777
  , Socket.cSocketOptions = [(ReuseAddr, 1)]
  , Socket.cConnections   = 1
  , Socket.cAddrInfo      = Just (defaultHints {addrFlags = [AI_PASSIVE], addrAddress=SockAddrUnix "0.0.0.0"})
  , Socket.cHostname      = Nothing
  , Socket.cServiceName   = Just "7777"
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
  state    <- atomically $ newTVar (GameState [] M.empty world playerMap M.empty M.empty M.empty)
  wChannel <- newTChanIO
  let env = Env state wChannel
  launchApp dBConfig socketConfig env
