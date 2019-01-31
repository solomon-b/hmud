{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM hiding (stateTVar)
--import Control.Exception (bracket)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Data.List (find)
import qualified Data.Map.Strict as M
import Database.SQLite.Simple (open)
import Network.Socket

import Dispatch
import State
import Prompts
import SqliteLib (getUsersDb)
import Types ( Env(..)
             , GlobalState(..)
             , ThreadEnv(..)
             , User(..)
             )
import World


userLoop :: ReaderT ThreadEnv IO ()
userLoop = forever $ do
    state <- readState
    thread <- liftIO myThreadId
    readTChanLoop
    let user :: Maybe (User, ThreadId)
        user = find (\(_, tid) -> tid == thread) (globalActiveUsers state)

    case user of
        Just _ -> gamePrompt
        Nothing -> mainMenuPrompt

mainLoop :: ReaderT Env IO ()
mainLoop = forever $ do
    stateTVar <- asks envStateTVar
    conn <- asks envConn
    sock <- asks envSock
    wChannel<- asks envWChannel
    rChannel <- liftIO . atomically $ dupTChan wChannel
    userIdTvar <- liftIO . atomically $ newTVar Nothing
    (sock', _) <- lift $ accept sock
    users <- asks envUsers

    drainTChanLoop rChannel

    void . liftIO $ do
        putStrLn "Got connection, handling query"
        let threadEnv = ThreadEnv conn sock' stateTVar wChannel rChannel userIdTvar users
        forkIO $ runReaderT userLoop threadEnv

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

main :: IO ()
main = withSocketsDo $ do
    conn <- open "hmud.db"
    gameSock <- createSocket 78
    state <- atomically $ newTVar (GlobalState M.empty world playerMap)
    wChannel <- newTChanIO
    users <- liftIO $ getUsersDb conn
    let env = Env conn gameSock state wChannel users
    runReaderT mainLoop env 
