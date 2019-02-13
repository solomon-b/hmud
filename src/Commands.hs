{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Commands where

import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T

import Errors
import Room
import Parser
import SqliteLib
import State
import Types 
    ( GameState(..)
    , HasConnectionHandle(..)
    , MonadGameState(..)
    , MonadPlayer(..)
    , MonadTCP(..)
    , MonadThread(..)
    , MonadTChan(..)
    , RoomText(..)
    , Response(..)
    , UserEnv(..)
    )
import World

execCommand ::
    ( MonadReader UserEnv m
    , MonadIO m
    , MonadTCP m
    , MonadTChan m
    , MonadPlayer m
    , MonadGameState m
    , MonadThread m
    ) => Command -> m (Either AppError Response)
execCommand GetUsers = execGetUsers
execCommand (GetUser username) = execGetUser username
execCommand (AddUser user) = execAddUser user
execCommand (Echo msg) = execEcho msg
execCommand Exit = execExit
execCommand Logout = execLogout
execCommand Shutdown = execShutdown
execCommand Whois = execWhois
execCommand (Say msg) = execSay msg
execCommand (Move dir) = execMovePlayer dir
execCommand Look = execShowRoom
execCommand _ = return $ Left InvalidCommand


execGetUsers ::
    ( MonadReader UserEnv m
    , MonadIO m
    ) => m (Either AppError Response)
execGetUsers = do
    conn <- asks getConnectionHandle
    users <- liftIO $ getUsersDb conn
    let usernames = userUsername <$> users 
        newlineSeperated = T.concat $ intersperse "\n" usernames ++ pure (T.pack "\r\n")
    return . Right $ RespAnnounce newlineSeperated

execGetUser ::
    ( MonadReader UserEnv m
    , MonadIO m
    ) => Text -> m (Either AppError Response)
execGetUser username = do
    conn <- asks getConnectionHandle
    eUser <- liftIO $ selectUser conn (T.strip username)
    case eUser of
        Left err' -> return $ Left err'
        Right user' -> return . Right . RespAnnounce $ formatUser user'

execAddUser ::
    ( MonadReader UserEnv m
    , MonadIO m
    ) => User -> m (Either AppError Response)
execAddUser user = do
    conn <- asks getConnectionHandle
    result <- liftIO $ addUserDb conn user
    return . Right $ RespAnnounce result

execEcho :: 
    ( Monad m
    ) => Text -> m (Either AppError Response)
execEcho = return . Right . RespAnnounce

execExit ::
    ( MonadReader UserEnv m
    , MonadIO m
    , MonadGameState m
    , MonadThread m
    , MonadPlayer m
    ) => m (Either AppError Response)
execExit = do
    void execLogout 
    return . Right $ RespAnnounce "Goodbye!"

execLogout ::
    ( MonadReader UserEnv m
    , MonadIO m
    , MonadGameState m
    , MonadThread m
    , MonadPlayer m
    ) => m (Either AppError Response)
execLogout = do
    (GameState activePlayers _ playerMap') <- readState
    eUser <- getUser
    case eUser of
        Left err -> return $ Left err
        Right user -> do
            let userId = userUserId user
                activePlayers' = removeUser userId activePlayers
            setState $ GameState activePlayers' world playerMap'
            return . Right $ RespAnnounce "Logged Out"

execShutdown ::
    (Monad m) => m (Either AppError Response)
execShutdown = return . Right $ RespAnnounce "Shutting Down! Goodbye!" 

execWhois ::
    (MonadGameState m) => m (Either AppError Response)
execWhois = Right . RespAnnounce . whois <$> readState

execSay :: (MonadPlayer m) => Text -> m (Either AppError Response)
execSay msg = do
    eUser <- getUser
    case eUser of
        Left err -> return $ Left err
        Right user -> return . Right $ RespSay (userUsername user) msg

execMovePlayer ::
    ( MonadReader UserEnv m
    , MonadIO m
    , MonadGameState m
    , MonadThread m
    , MonadPlayer m
    ) => Direction -> m (Either AppError Response)
execMovePlayer dir = do
    state <- readState
    eCurrentRoom <- getUserLocation
    eUser <- getUser
    
    case eUser of
        Left err -> return $ Left err
        Right (User uid _ _) -> do
            let eUidNewRid  = (,) <$> pure uid <*> destRoomId eCurrentRoom dir
            case eUidNewRid of
                Left NoSuchRoom -> return . Right $ RespAnnounce "There is no path in that direction"
                Left err -> return $ Left err
                Right (_, newRid) -> do
                    let playerMap' = findAndSwapPlayer uid newRid (globalPlayerMap state)
                    setState $ replacePlayerMap state playerMap'
                    return . Right $ RespAnnounce "You have moved into a new room..."

execShowRoom ::
    ( MonadReader UserEnv m
    , MonadIO m
    , MonadGameState m
    , MonadPlayer m
    , MonadThread m
    ) => m (Either AppError Response)
execShowRoom = do
    globalState' <- readState
    eRoom        <- getUserLocation
    eUser        <- getUser
    let res = do
            (User uid _ _)  <- eUser
            room <- eRoom
            showRoom' uid room globalState'
    case res of
        Left  err      -> return $ Left err
        Right roomText -> return . Right . RespAnnounce $ getRoomText roomText
