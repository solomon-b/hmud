{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Prompts where
-- This module will hold all command prompts

import Control.Concurrent.STM
import Control.Monad (void)
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)

import Account
import Commands
import Dispatch
import Errors
import Parser
import State
import SqliteLib
import Types 
    ( GameState(..)
    , HasConnectionHandle(..)
    , MonadGameState(..)
    , MonadThread(..)
    , MonadPrompt(..)
    , MonadTChan(..)
    , UserEnv(..)
    , Response(..)
    )
import World


test :: MonadPrompt m => m ByteString
test = prompt "$ "

mainMenuPrompt ::
    ( MonadReader UserEnv m
    , MonadIO m
    , MonadThread m
    , MonadGameState m
    , MonadPrompt m
    , MonadTChan m
    ) => m ()
mainMenuPrompt = do
    respTChan <- asks userEnvRespTchan
    mapM_ (writeChannel respTChan . RespAnnounce) ["Welcome to hMud", "Options: register, login, exit"]
    
    eCommand <- runMainMenuParse <$> prompt "> "
    case eCommand of
        Left _         -> writeChannel respTChan (RespAnnounce "Invalid Command") >> mainMenuPrompt
        Right Exit     -> return () -- TODO: Integrate Exit
        Right Login    -> loginPrompt
        Right Register -> return () -- TODO: Integrate Registration Func
        _              -> return ()

loginPrompt :: 
    ( MonadReader UserEnv m
    , MonadIO m
    , MonadGameState m
    , MonadPrompt m
    , MonadTChan m
    ) => m ()
loginPrompt = do
    uidTVar   <- asks userEnvUserId
    respTChan <- asks userEnvRespTchan
    conn      <- asks getConnectionHandle
    users     <- liftIO $ getUsersDb conn
    (GameState activeUsers _ playerMap') <- readState

    parsedUser     <- runWordParse <$> prompt "Login: "
    suppressEcho
    parsedPassword <- runWordParse <$> prompt "Password: "
    unsuppressEcho
    let loginResult :: Either AppError User
        loginResult = do
            username <- parsedUser
            password <- parsedPassword
            user <- checkLogin users username
            checkPassword password user
    case loginResult of
        Left err'  -> do
            liftIO (print err') 
            writeChannel respTChan (RespAnnounce . T.pack $ show err') 
            loginPrompt
        Right user ->
            if userIsLoggedIn activeUsers (userUserId user) 
            then loginPrompt
            else do 
                let uid = userUserId user
                    activeUsersMap = M.insert (userUserId user) user activeUsers
                    playerMap'' = addPlayer uid 1 playerMap'
                liftIO . atomically $ writeTVar uidTVar (Just uid)
                setState $ GameState activeUsersMap world playerMap''
                liftIO $ print $ userUsername user `T.append` " Logged In"
                writeChannel respTChan $ RespAnnounce "\r\nLogin Succesful"

usernameRegPrompt ::
    ( MonadReader UserEnv m
    , MonadIO m
    , MonadPrompt m
    ) => m (Either AppError Text)
usernameRegPrompt = do
    conn <- asks getConnectionHandle
    users <- liftIO $ getUsersDb conn
    usernameBS <- prompt "username: "
    return $ validateUsername users usernameBS

passwordRegPrompt ::
    ( MonadReader UserEnv m
    , MonadPrompt m
    ) => m (Either AppError Text)
passwordRegPrompt = do
    suppressEcho
    passwordBS  <- prompt "password: "
    passwordBS' <- prompt "repeat password: "
    unsuppressEcho
    return $ validatePassword passwordBS passwordBS'

registerPrompt ::
    ( MonadReader UserEnv m
    , MonadIO m
    , MonadPrompt m
    ) => m ()
registerPrompt = do
    conn <- asks getConnectionHandle
    users <- liftIO $ getUsersDb conn
    
    usernameBS <- prompt "username: "
    let usernameE = validateUsername users usernameBS
    
    case usernameE of
        Left err -> liftIO (print err) >> registerPrompt
        Right username -> do
            passwordM <- passwordRegPrompt
            case passwordM of
                -- TODO: Only require the user to re-enter password
                Left err   -> liftIO (print err) >> registerPrompt
                Right pass -> void . liftIO $ addUserDb conn (User 0 username pass) 

gamePrompt :: 
    ( MonadReader UserEnv m
    , MonadIO m
    , MonadPrompt m
    , MonadGameState m
    ) => m (Either AppError Response)
gamePrompt = do
    env <- ask
    cmd <- prompt "> "
    let cmdParse = runParse cmd
    liftIO $ print cmdParse
    case cmdParse of
        Right cmd' -> runReaderT (execCommand cmd') env
        Left err'  -> return $ Left err'
        --sendMsg "Command not recognized" >> liftIO (print err')
