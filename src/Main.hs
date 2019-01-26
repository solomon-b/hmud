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
--import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, open)
import qualified Database.SQLite.Simple as SQLite
import Network.Socket
import System.Exit (exitSuccess)
import Text.Trifecta (parseByteString)

import Commands
import Dispatch
import SqliteLib
import State
import TelnetLib (prompt)
import Types ( Command(..)
             , Env(..)
             , Error(..)
             , GlobalState(..)
             , ThreadEnv(..)
             , User(..)
             , Direction(..)
             , Room(..)
             , PlayerMap
             , RoomId
             , UserId
             )
import Parser
import World


------------------------
---- Misc Functions ----
------------------------

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing = Left a


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
validatePassword :: ByteString -> ByteString -> ReaderT ThreadEnv IO (Maybe Text)
validatePassword pass1BS pass2BS = do
    let parsedPass1 = resultToEither $ parseByteString word mempty pass1BS
    let parsedPass2 = resultToEither $ parseByteString word mempty pass2BS
    case (parsedPass1, parsedPass2) of
        (Right pass1, Right pass2) | pass1 == pass2 -> return $ Just pass1
        _ -> return Nothing
    
validateUsername :: ByteString -> ReaderT ThreadEnv IO (Maybe Text)
validateUsername usernameBS = do
    conn <- asks threadEnvConn
    let parsedUsername = resultToEither $ parseByteString word mempty usernameBS
    case parsedUsername of
        Left _ -> return Nothing
        Right username -> do
            eUser <- liftIO $ selectUser conn (T.strip username)
            case eUser of
                Right _ -> return Nothing
                Left _ -> return $ Just username

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

getUserId :: ReaderT ThreadEnv IO (Either Error UserId)
getUserId = do
    activeUsers <- globalActiveUsers <$> readState
    thread <- liftIO myThreadId
    return $ userUserId <$> getUserByThread thread activeUsers


--------------------------
---- Player Movement  ----
--------------------------

removeFromPlayerMap :: UserId -> RoomId -> PlayerMap -> PlayerMap
removeFromPlayerMap uid = 
    M.adjust (filter (/= uid)) 

addToPlayerMap :: UserId -> RoomId -> PlayerMap -> PlayerMap
addToPlayerMap uid = 
    M.adjust ((:) uid)

swapInPlayerMap :: UserId -> RoomId -> RoomId -> PlayerMap -> PlayerMap
swapInPlayerMap uid rid rid' =
    addToPlayerMap uid rid' . removeFromPlayerMap uid rid

findInPlayerMap :: UserId -> PlayerMap -> Maybe (RoomId, UserId)
findInPlayerMap uid playerMap' = 
    let f (i, xs) = fmap ((,) i) xs
        players = concatMap f (M.toList playerMap')
    in find (\(_, uid') -> uid == uid') players

adjustPlayerMap :: UserId -> RoomId -> PlayerMap -> PlayerMap
adjustPlayerMap uid rid playerMap' =
    case findInPlayerMap uid playerMap' of
        Nothing -> addToPlayerMap uid rid playerMap'
        Just (rid', _) -> swapInPlayerMap uid rid' rid playerMap'

adjustPlayerLocation :: UserId -> RoomId -> ReaderT ThreadEnv IO ()
adjustPlayerLocation uid rid = do
    (GlobalState activeUsers w playerMap') <- readState
    liftIO $ print $ "uid: " ++ show uid
    liftIO $ print $ "active users: " ++ show activeUsers
    case activeUsers M.!? uid of
        Nothing -> liftIO $ putStrLn "User not found"
        Just _ -> 
            let playerMap'' = adjustPlayerMap uid rid playerMap'
            in do
                liftIO $ print playerMap''
                setState (GlobalState activeUsers w playerMap'')

getUserLocation :: ReaderT ThreadEnv IO (Either Error Room)
getUserLocation = do
    playerMap' <- globalPlayerMap <$> readState
    uidTVar <- asks threadEnvUserId 
    mUid <- liftIO $ readTVarIO uidTVar
    case mUid of
        Nothing -> liftIO . pure $ Left NotLoggedIn
        Just uid ->
            case findInPlayerMap uid playerMap' of
                Nothing -> liftIO . pure $ Left UserHasNoLocation
                Just (rid, _) -> do 
                    liftIO . putStrLn $ "user is in room: " ++ show rid
                    liftIO . pure . Right $ world M.! rid

movePlayer :: Direction -> ReaderT ThreadEnv IO ()
movePlayer dir = do
    eRoom <- getUserLocation
    case eRoom of
        Left err -> liftIO $ print err
        Right room -> do
            eUid <- getUserId 
            case eUid of
                Left err -> liftIO $ print err
                Right uid ->
                    case roomAdjacent room M.!? dir of
                        Nothing -> do
                            liftIO $ putStrLn "No such room"
                            sendMsg "There is no path in that direction"
                        Just newRid -> do
                            adjustPlayerLocation uid newRid
                            showRoom

spawnPlayer :: ReaderT ThreadEnv IO ()
spawnPlayer = do
    liftIO $ putStrLn "Spawning Player.."
    uidTVar <- asks threadEnvUserId 
    mUid <- liftIO $ readTVarIO uidTVar
    case mUid of
        Nothing -> liftIO $ putStrLn "user is not logged in"
        Just uid -> adjustPlayerLocation uid 1 >> liftIO (putStrLn "..Player Spawned")
    
getUsersInRoom :: RoomId -> ReaderT ThreadEnv IO (Either Error [User])
getUsersInRoom rid = do
    (GlobalState activeUsers' _ playerMap') <- readState
    eUid <- getUserId
    let usersInRoom = do
        uids <- maybeToEither NoSuchRoom $ playerMap' M.!? rid
        uid <- eUid
        let users = fmap (\uid' -> fst $ activeUsers' M.! uid') uids
            filteredUsers = filter (\(User userId _ _) -> userId /= uid) users
        return filteredUsers
    return usersInRoom
        
showUsersInRoom' :: Room -> [User] -> ReaderT ThreadEnv IO ()
showUsersInRoom' room users = do
    sendMsg $ roomName room
    sendMsg $ roomDescription room
    sendMsg . T.pack $ "You see: " ++ show users
    sendMsg . T.pack $ "Exits: " ++ show (M.keys $ roomAdjacent room)

showUsersInRoom :: RoomId -> ReaderT ThreadEnv IO [User]
showUsersInRoom rid = do
    (GlobalState activeUsers' _ playerMap') <- readState
    let eUids = maybeToEither NoSuchRoom $ playerMap' M.!? rid
    case eUids of
        Left _ -> return []
        Right uids -> do
            eUid <- getUserId
            case eUid of
                Left err -> liftIO $ print err >> return []
                Right uid -> 
                    let users = fmap (\uid' -> fst $ activeUsers' M.! uid') uids
                        filteredUsers = filter (\(User userId _ _) -> userId /= uid) users
                    in return filteredUsers

showRoom :: ReaderT ThreadEnv IO ()
showRoom = do
    eRoom <- getUserLocation
    case eRoom of
        Left err -> liftIO $ print err
        Right room -> do
            eUsers <- getUsersInRoom $ roomRoomId room
            case eUsers of
                Left err -> liftIO $ print err
                Right users -> showUsersInRoom' room users
     

-----------------
---- Prompts ----
-----------------

mainMenuPrompt :: ReaderT ThreadEnv IO ()
mainMenuPrompt = do
    sock <- asks threadEnvSock
    mapM_ sendMsg ["Welcome to hMud", "Options: register, login, exit"]
    
    eCommand <- liftIO $ runParse <$> prompt sock "> "
    case eCommand of
        Left _ -> sendMsg "Invalid Command" >> mainMenuPrompt 
        Right Exit -> return ()
        Right Login -> loginPrompt
        Right Register -> return ()
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

-- TODO: Refactor and simplify:
registerPrompt :: ReaderT ThreadEnv IO ()
registerPrompt = do
    conn <- asks threadEnvConn
    sock <- asks threadEnvSock
    
    usernameBS <- liftIO $ prompt sock "username: "
    usernameM <- validateUsername usernameBS
    
    case usernameM of
        Nothing -> registerPrompt
        Just username -> do
            suppressEcho
            passwordBS <- liftIO $ prompt sock "password: "
            passwordBS' <- liftIO $ prompt sock "repeat password: "
            unsuppressEcho

            passwordM <- validatePassword passwordBS passwordBS'
            case passwordM of
                -- TODO: Only require the user to re-enter password
                Nothing -> registerPrompt
                Just pass -> void . liftIO $ addUserDb conn (User 0 username pass) 

-- TODO: Refactor and simplify:
gamePrompt :: Maybe (User, ThreadId) -> ReaderT ThreadEnv IO ()
gamePrompt Nothing = loginPrompt
gamePrompt (Just (user, _)) = do
    stateTVar <- asks threadEnvStateTVar
    conn <- asks threadEnvConn
    sock <- asks threadEnvSock
    state <- liftIO $ readTVarIO stateTVar

    cmd <- liftIO $ prompt sock "> "
    let cmdParse = runParse cmd
    liftIO $ print cmdParse
    case cmdParse of
        Right GetUsers -> liftIO (getUsersDb conn) >>= sendMsg
        Right (GetUser user') -> liftIO (getUserDb conn user') >>= sendMsg
        Right (AddUser user') -> liftIO (addUserDb conn user') >>= sendMsg
        Right (Echo msg) -> sendMsg msg
        Right Exit -> execExit
        Right Logout -> execLogout
        Right Shutdown -> do
            sendMsg "Shutting Down! Goodbye!" 
            liftIO (SQLite.close conn >> close sock >> exitSuccess)
        Right Whois -> sendMsg (whois state)
        Right (Say msg) -> broadcast (T.concat ["<", userUsername user, "> ", msg])
        Right (Move dir) -> movePlayer dir
        Right Look -> showRoom
        Right _ -> return ()
        Left err' -> sendMsg "Command not recognized" >> liftIO (print err')


--------------
---- Main ----
--------------

userLoop :: ReaderT ThreadEnv IO ()
userLoop = do
    state <- readState
    thread <- liftIO myThreadId
    readTChanLoop
    let user = find (\(_, tid) -> tid == thread) (globalActiveUsers state)

    case user of
        Just _ -> gamePrompt user >> userLoop
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
