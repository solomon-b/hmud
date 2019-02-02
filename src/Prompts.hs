{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Prompts where
-- This module will hold all command prompts

import Control.Concurrent.STM
--import Control.Exception (bracket)
import Control.Monad (void)
import qualified Control.Monad.Reader as R
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

import Account
import Commands
import Dispatch
import Parser
import State
import SqliteLib
import Types ( Command(..)
             , AppError(..)
             , GameState(..)
             , MonadGameState(..)
             , MonadThread(..)
             , MonadPrompt(..)
             , ThreadEnv(..)
             , User(..)
             )
import World

mainMenuPrompt ::
    ( R.MonadReader ThreadEnv m
    , R.MonadIO m
    , MonadThread m
    , MonadGameState m
    , MonadPrompt m
    ) => m ()
mainMenuPrompt = do
    mapM_ sendMsg' ["Welcome to hMud", "Options: register, login, exit"]
    
    eCommand <- runMainMenuParse <$> prompt "> "
    case eCommand of
        Left _ -> sendMsg' "Invalid Command" >> mainMenuPrompt
        Right Exit -> return ()
        Right Login -> loginPrompt
        Right Register -> return () -- TODO: Integrate Registration Func
        _  -> return ()

loginPrompt :: 
    ( R.MonadReader ThreadEnv m
    , R.MonadIO m
    , MonadThread m
    , MonadGameState m
    , MonadPrompt m
    ) => m ()
loginPrompt = do
    (ThreadEnv _ _ _ _ _ uidTVar users) <- R.ask
    (GameState activeUsers _ playerMap') <- readState'
    thread <- getThread

    parsedUser     <- runWordParse <$> prompt "Login: "
    suppressEcho
    parsedPassword <- runWordParse <$> prompt "Password: "
    unsuppressEcho
    let loginResult = do
            username <- parsedUser
            password <- parsedPassword
            user <- checkLogin users username
            checkPassword password user
    case loginResult of
        Left err'  -> liftIO (print err') >> sendMsg' (T.pack $ show err') >> loginPrompt
        Right user ->
            if userIsLoggedIn activeUsers (userUserId user) 
            then loginPrompt
            else do 
                let uid = userUserId user
                    activeUsersMap = M.insert (userUserId user) (user, thread) activeUsers
                    playerMap'' = addPlayer uid 1 playerMap'
                liftIO . atomically $ writeTVar uidTVar (Just uid)
                setState' $ GameState activeUsersMap world playerMap''
                liftIO $ print $ userUsername user `T.append` " Logged In"
                sendMsg' "\r\nLogin Succesful"

usernameRegPrompt ::
    ( R.MonadReader ThreadEnv m
    , R.MonadIO m
    , MonadPrompt m
    ) => m (Either AppError Text)
usernameRegPrompt = do
    users <- R.asks threadEnvUsers -- TODO: Figure out why this isnt in a TVar
    usernameBS <- prompt "username: "
    return $ validateUsername users usernameBS

passwordRegPrompt ::
    ( R.MonadReader ThreadEnv m
    , MonadPrompt m
    ) => m (Either AppError Text)
passwordRegPrompt = do
    suppressEcho
    passwordBS  <- prompt "password: "
    passwordBS' <- prompt "repeat password: "
    unsuppressEcho
    return $ validatePassword passwordBS passwordBS'

registerPrompt ::
    ( R.MonadReader ThreadEnv m
    , R.MonadIO m
    , MonadPrompt m
    ) => m ()
registerPrompt = do
    conn  <- R.asks threadEnvConn
    users <- R.asks threadEnvUsers
    
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
    ( R.MonadReader ThreadEnv m
    , R.MonadIO m
    , MonadPrompt m
    ) => m ()
gamePrompt = do
    env <- R.ask
    cmd  <- prompt "> "
    let cmdParse = runParse cmd
    liftIO $ print cmdParse
    case cmdParse of
        Right cmd' -> liftIO $ runReaderT (execCommand cmd') env
        Left err'  -> sendMsg' "Command not recognized" >> liftIO (print err')
