{-# LANGUAGE OverloadedStrings #-}
module Prompts where
-- This module will hold all command prompts

import Control.Concurrent
import Control.Concurrent.STM
--import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

import Account
import Commands
import Dispatch
import Parser
import Room
import State
import SqliteLib
import TelnetLib (prompt)
import Types ( Command(..)
             , Error(..)
             , GlobalState(..)
             , ThreadEnv(..)
             , User(..)
             )
import World


mainMenuPrompt :: ReaderT ThreadEnv IO ()
mainMenuPrompt = do
    sock <- asks threadEnvSock
    mapM_ sendMsg ["Welcome to hMud", "Options: register, login, exit"]
    
    eCommand <- liftIO $ runMainMenuParse <$> prompt sock "> "
    case eCommand of
        Left _ -> sendMsg "Invalid Command" >> mainMenuPrompt 
        Right Exit -> return ()
        Right Login -> loginPrompt
        Right Register -> return () -- TODO: Integrate Registration Func
        _  -> return ()

-- TODO: Refactor and simplify:
loginPrompt :: ReaderT ThreadEnv IO ()
loginPrompt = do
    (ThreadEnv _ sock _ _ _ uidTVar users) <- ask
    (GlobalState activeUsers _ playerMap') <- readState
    thread <- liftIO myThreadId

    parsedUser <- liftIO $ runWordParse <$> prompt sock "Login: "
    suppressEcho
    parsedPassword <- liftIO $ runWordParse <$> prompt sock "Password: "
    unsuppressEcho
    let loginResult = do
            username <- parsedUser
            password <- parsedPassword
            user <- checkLogin users username
            checkPassword password user
    case loginResult of
        Left err' -> liftIO (print err') >> sendMsg (T.pack $ show err') >> loginPrompt
        Right user ->
            if userIsLoggedIn activeUsers (userUserId user) 
            then loginPrompt
            else do 
                let activeUsersMap = M.insert (userUserId user) (user, thread) activeUsers
                liftIO . atomically $ writeTVar uidTVar (Just $ userUserId user)
                setState $ GlobalState activeUsersMap world playerMap'
                liftIO $ print $ userUsername user `T.append` " Logged In"
                sendMsg "\r\nLogin Succesful"
                spawnPlayer

usernameRegPrompt :: ReaderT ThreadEnv IO (Either Error Text)
usernameRegPrompt = do
    sock <- asks threadEnvSock
    users <- asks threadEnvUsers
    usernameBS <- liftIO $ prompt sock "username: "
    return $ validateUsername users usernameBS

passwordRegPrompt :: ReaderT ThreadEnv IO (Either Error Text)
passwordRegPrompt = do
    sock <- asks threadEnvSock
    suppressEcho
    passwordBS <- liftIO $ prompt sock "password: "
    passwordBS' <- liftIO $ prompt sock "repeat password: "
    unsuppressEcho
    return $ validatePassword passwordBS passwordBS'

registerPrompt :: ReaderT ThreadEnv IO ()
registerPrompt = do
    conn <- asks threadEnvConn
    sock <- asks threadEnvSock
    users <- asks threadEnvUsers
    
    usernameBS <- liftIO $ prompt sock "username: "
    let usernameE = validateUsername users usernameBS
    
    case usernameE of
        Left err -> liftIO (print err) >> registerPrompt
        Right username -> do
            passwordM <- passwordRegPrompt
            case passwordM of
                -- TODO: Only require the user to re-enter password
                Left err -> liftIO (print err) >> registerPrompt
                Right pass -> void . liftIO $ addUserDb conn (User 0 username pass) 

-- TODO: Refactor and simplify:
gamePrompt :: ReaderT ThreadEnv IO ()
gamePrompt = do
    sock <- asks threadEnvSock
    cmd <- liftIO $ prompt sock "> "
    let cmdParse = runParse cmd
    liftIO $ print cmdParse
    case cmdParse of
        Right cmd' -> execCommand cmd'
        Left err' -> sendMsg "Command not recognized" >> liftIO (print err')
