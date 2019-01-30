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
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, open)
import Network.Socket
import Text.Trifecta (parseByteString)

import Commands
import Dispatch
import Room
import SqliteLib
import State
import TelnetLib (prompt)
import Types ( Command(..)
             , Env(..)
             , Error(..)
             , GlobalState(..)
             , ThreadEnv(..)
             , User(..)
             , UserId
             )
import Parser
import World


---------------------------
---- Account Functions ----
---------------------------

-- I wish this worked:
--checkLogin' :: Connection -> Either Text Text -> Either Text Text -> IO (Either Text User)
--checkLogin' conn acc pass = do
--    acc' <- acc
--    pass' <- pass
--    eUser  <- selectUser conn acc'
--    user <- eUser
--    return $ checkPassword pass' user

checkPassword :: Text -> User -> Either Error User
checkPassword pass acc
    | pass /= userPassword acc = Left InvalidPassword
    | otherwise = Right acc

checkLogin :: Connection -> Either Error Text -> Either Error Text -> IO (Either Error User)
checkLogin _ (Left err') _ = print err' >> return (Left NoSuchUser)
checkLogin _ _ (Left err') = print err' >> return (Left InvalidPassword)
checkLogin conn (Right acc) (Right pass) = do

    eUser <- selectUser conn acc 
    return $ eUser >>= checkPassword pass 

-- TODO: Add minimum password strength req
validatePassword :: ByteString -> ByteString -> Either Error Text
validatePassword pass1BS pass2BS = do
    let parsedPass1 = resultToEither $ parseByteString word mempty pass1BS
    let parsedPass2 = resultToEither $ parseByteString word mempty pass2BS
    case (parsedPass1, parsedPass2) of
        (Right pass1, Right pass2) | pass1 == pass2 -> Right pass1
                                   | otherwise -> Left PasswordsDontMatch
        (Right _, Left err2) -> Left . BadParse $ show err2
        (Left err1, _) -> Left . BadParse $ show err1
    
validateUsername :: ByteString -> ReaderT ThreadEnv IO (Either Error Text)
validateUsername usernameBS = do
    conn <- asks threadEnvConn
    let parsedUsername = resultToEither $ parseByteString word mempty usernameBS
    case parsedUsername of
        Left err -> return . Left . BadParse $ show err
        Right username -> do
            eUser <- liftIO $ selectUser conn (T.strip username)
            case eUser of
                Right _ -> return $ Left NoSuchUser
                Left _ -> return $ Right username

userIsLoggedIn :: UserId -> ReaderT ThreadEnv IO ()
userIsLoggedIn userId = do
    curState <- readState
    let stateMap = globalActiveUsers curState
        mUser = M.lookup userId stateMap
    case mUser of
        Just _ -> do
            sendMsg "You are already logged in!"
            loginPrompt
        Nothing -> return ()


--------------------------
---- Player Movement  ----
--------------------------


-----------------
---- Prompts ----
-----------------

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
    (ThreadEnv conn sock _ _ _ uidTVar) <- ask
    (GlobalState activeUsers _ playerMap') <- readState
    thread <- liftIO myThreadId

    parsedUser <- liftIO $ runWordParse <$> prompt sock "Login: "
    suppressEcho
    parsedPassword <- liftIO $ runWordParse <$> prompt sock "Password: "
    unsuppressEcho

    loginResult <- liftIO $ checkLogin conn parsedUser parsedPassword
    case loginResult of
        Left err' -> liftIO (print err') >> sendMsg (T.pack $ show err') >> loginPrompt
        Right user -> do
            userIsLoggedIn (userUserId user)
            let activeUsersMap = M.insert (userUserId user) (user, thread) activeUsers
            liftIO . atomically $ writeTVar uidTVar (Just $ userUserId user)
            setState $ GlobalState activeUsersMap world playerMap'
            liftIO $ print $ userUsername user `T.append` " Logged In"
            sendMsg "\r\nLogin Succesful"
            spawnPlayer
            userLoop

usernameRegPrompt :: ReaderT ThreadEnv IO (Either Error Text)
usernameRegPrompt = do
    sock <- asks threadEnvSock
    usernameBS <- liftIO $ prompt sock "username: "
    validateUsername usernameBS

passwordRegPrompt :: ReaderT ThreadEnv IO (Either Error Text)
passwordRegPrompt = do
    sock <- asks threadEnvSock
    suppressEcho
    passwordBS <- liftIO $ prompt sock "password: "
    passwordBS' <- liftIO $ prompt sock "repeat password: "
    unsuppressEcho
    return $ validatePassword passwordBS passwordBS'

-- TODO: Refactor and simplify:
registerPrompt :: ReaderT ThreadEnv IO ()
registerPrompt = do
    conn <- asks threadEnvConn
    sock <- asks threadEnvSock
    
    usernameBS <- liftIO $ prompt sock "username: "
    usernameM <- validateUsername usernameBS
    
    case usernameM of
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


--------------
---- Main ----
--------------

userLoop :: ReaderT ThreadEnv IO ()
userLoop = do
    state <- readState
    thread <- liftIO myThreadId
    readTChanLoop
    let user :: Maybe (User, ThreadId)
        user = find (\(_, tid) -> tid == thread) (globalActiveUsers state)

    case user of
        Just _ -> gamePrompt >> userLoop
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

    drainTChanLoop rChannel

    void . liftIO $ do
        putStrLn "Got connection, handling query"
        let threadEnv = ThreadEnv conn sock' stateTVar wChannel rChannel userIdTvar
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
    let env = Env conn gameSock state wChannel
    runReaderT mainLoop env 
