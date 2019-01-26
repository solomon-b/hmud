{-# LANGUAGE OverloadedStrings #-}
module Commands where

import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import qualified Database.SQLite.Simple as SQLite
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket
import System.Exit (exitSuccess)

import Dispatch
import SqliteLib
import State
import Types ( Command(..)
             , GlobalState(..)
             , ThreadEnv(..)
             , User(..)
             )
import World

execCommand :: Command ->  ReaderT ThreadEnv IO ()
execCommand GetUsers = execGetUsers
execCommand (GetUser username) = execGetUser username
execCommand (AddUser user) = execAddUser user
execCommand (Echo msg) = execEcho msg
execCommand Exit = execExit
execCommand Logout = execLogout
execCommand Shutdown = execShutdown
execCommand Whois = execWhois
execCommand (Say msg) = execSay msg
--execCommand (Move dir) = execMovePlayer dir
--execCommand Look = showRoom
execCommand _ = return ()


execGetUsers :: ReaderT ThreadEnv IO ()
execGetUsers = do
    conn <- asks threadEnvConn
    users <- liftIO $ getUsersDb conn
    sendMsg users

execGetUser :: Text -> ReaderT ThreadEnv IO ()
execGetUser username = do
    conn <- asks threadEnvConn
    eUser <- liftIO $ selectUser conn (T.strip username)
    case eUser of
        Left err' -> liftIO (print err') >> sendMsg "Problem finding user"
        Right user' -> sendMsg $ formatUser user'

execAddUser :: User -> ReaderT ThreadEnv IO ()
execAddUser user = do
    conn <- asks threadEnvConn
    result <- liftIO $ addUserDb conn user
    sendMsg result

execEcho :: Text -> ReaderT ThreadEnv IO ()
execEcho = sendMsg

execExit :: ReaderT ThreadEnv IO ()
execExit = do
    sock <- asks threadEnvSock
    execLogout 
    sendMsg "Goodbye!" 
    liftIO (close sock)

execLogout :: ReaderT ThreadEnv IO ()
execLogout = do
    (GlobalState activePlayers _ playerMap') <- readState
    threadId <- liftIO myThreadId
    case getUserByThread threadId activePlayers of
        Left _ -> return ()
        Right user -> do
            let userId = userUserId user
                activePlayers' = removeUser userId activePlayers
            setState $ GlobalState activePlayers' world playerMap'

execShutdown :: ReaderT ThreadEnv IO ()
execShutdown = do
    conn <- asks threadEnvConn
    sock <- asks threadEnvSock
    sendMsg "Shutting Down! Goodbye!" 
    liftIO (SQLite.close conn >> close sock >> exitSuccess)

execWhois :: ReaderT ThreadEnv IO ()
execWhois = do
    state <- readState 
    sendMsg $ whois state

execSay :: Text -> ReaderT ThreadEnv IO ()
execSay msg = do
    activeUsers <- globalActiveUsers <$> readState
    threadId <- liftIO myThreadId
    case getUserByThread threadId activeUsers of
        Left _ -> return () -- TODO: How to report this error?
        Right user ->
            broadcast (T.concat ["<", userUsername user, "> ", msg])

