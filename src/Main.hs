{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM hiding (stateTVar)
--import Control.Exception (bracket)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Data.List (intersperse, find, intercalate)
import Data.ByteString (ByteString)
import Data.ByteString as BS (pack, append)
--import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.SQLite.Simple (Connection, open, query_)
import qualified Database.SQLite.Simple as SQLite
import Network.Socket
import Network.Socket.ByteString (sendAll)
import System.Exit (exitSuccess)
import Text.Trifecta (parseByteString)

import SqliteLib
import TelnetLib (prompt)
import Types ( Command(..)
             , Env(..)
             , GlobalState(..)
             , ThreadEnv(..)
             , User(..)
             , Direction(..)
             , Room(..)
             , ActiveUsers
             , PlayerMap
             , RoomId
             , UserId
             )
import Parser
import World


--------------------------
---- Server Functions ----
--------------------------

(+++) :: Text -> Text -> Text
(+++) = T.append

-- I wish this worked:
--checkLogin' :: Connection -> Either Text Text -> Either Text Text -> IO (Either Text User)
--checkLogin' conn acc pass = do
--    acc' <- acc
--    pass' <- pass
--    eUser  <- selectUser conn acc'
--    user <- eUser
--    return $ checkPassword pass' user

checkPassword :: Text -> User -> Either Text User
checkPassword pass acc
    | pass /= userPassword acc = Left "Invalid Password"
    | otherwise = Right acc

checkLogin :: Connection -> Either Text Text -> Either Text Text -> IO (Either Text User)
checkLogin _ (Left err') _ = print err' >> return (Left "Invalid User")
checkLogin _ _ (Left err') = print err' >> return (Left "Invalid Password")
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

--userAlreadyExists :: Text -> ReaderT ThreadEnv IO ()
--userAlreadyExists username = undefined

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

addUser :: Connection -> User -> IO Text
addUser conn (User _ username password _) = do
    eInserted <- insertUser conn [username, password]
    case eInserted of
        Left err' -> print err' >> return "Problem adding user"
        Right res -> return $ formatUser res

getUser :: Connection -> Text -> IO Text
getUser conn username = do
    eUser <- selectUser conn (T.strip username)
    case eUser of
        Left err' -> print err' >> return "Problem finding user"
        Right user' -> return $ formatUser user'

getUsers :: Connection -> IO Text
getUsers conn = do
    rows <- query_ conn selectUsersQuery
    let usernames = userUsername <$> rows
        newlineSeperated =
            T.concat $ intersperse "\n" usernames ++ pure (T.pack "\r\n")
    return newlineSeperated

logout :: User -> ReaderT ThreadEnv IO ()
logout currUser = do
    stateTVar <- asks threadEnvStateTVar
    (GlobalState activePlayers _ playerMap) <- liftIO $ readTVarIO stateTVar
    let activePlayers' = M.delete userId activePlayers
    setState $ GlobalState activePlayers' world playerMap
    where userId = userUserId currUser

readState :: ReaderT ThreadEnv IO GlobalState
readState = do
    stateTVar <- asks threadEnvStateTVar
    liftIO $ readTVarIO stateTVar

setState :: GlobalState -> ReaderT ThreadEnv IO ()
setState state = do
    stateTVar <- asks threadEnvStateTVar
    liftIO . atomically $ writeTVar stateTVar state

broadcast :: Text -> ReaderT ThreadEnv IO ()
broadcast msg = do
    wChannel <- asks threadEnvWChannel
    liftIO . atomically $ writeTChan wChannel msg

sendMsg :: Text -> ReaderT ThreadEnv IO ()
sendMsg msg = do
    sock <- asks threadEnvSock
    liftIO . sendAll sock . encodeUtf8 $ msg +++ "\r\n"

suppressEcho :: ReaderT ThreadEnv IO ()
suppressEcho = do
    sock <- asks threadEnvSock
    liftIO . print $ BS.append (encodeUtf8 "suppressing Echo: ") (BS.pack [255,251,1])
    liftIO . sendAll sock $ BS.pack [255,251,1]

unsuppressEcho :: ReaderT ThreadEnv IO ()
unsuppressEcho = do
    sock <- asks threadEnvSock
    liftIO . print $ BS.pack [255,252,1]
    liftIO . sendAll sock $ BS.pack [255,252,1]

forkReader :: ReaderT r IO () -> ReaderT r IO ThreadId
forkReader action = do
    env <- ask
    liftIO . forkIO $ runReaderT action env

readTChanLoop :: ReaderT ThreadEnv IO ()
readTChanLoop = void . forkReader . forever $ do
    rChannel <- asks threadEnvRChannel
    msg <- liftIO . atomically $ readTChan rChannel
    sendMsg msg

drainTChanLoop :: TChan a -> ReaderT r IO ()
drainTChanLoop rChannel =
    void . forkReader . forever . liftIO . atomically $ readTChan rChannel 

-- TODO: Cleanup this function:
whois :: GlobalState -> Text
whois curState = T.pack . intercalate ", " . fmap (show . fst) $ M.elems $ globalActiveUsers curState


--------------------------
---- Player Movement  ----
--------------------------

removeFromLocation :: UserId -> RoomId -> PlayerMap -> PlayerMap
removeFromLocation uid = 
    M.adjust (filter (/= uid)) 

addToLocation :: UserId -> RoomId -> PlayerMap -> PlayerMap
addToLocation uid = 
    M.adjust ((:) uid)

swapLocation :: UserId -> RoomId -> RoomId -> PlayerMap -> PlayerMap
swapLocation uid rid rid' =
    addToLocation uid rid' . removeFromLocation uid rid

userUpdateLocation :: User -> RoomId -> User
userUpdateLocation (User uid username pass _)= User uid username pass

updateActiveUsers :: ActiveUsers -> User -> ActiveUsers
updateActiveUsers activeUsers user =
    let uid = userUserId user 
        (_, thread) = activeUsers M.! uid
    in M.insert uid (user, thread) activeUsers

setPlayerLocation :: PlayerMap -> UserId -> RoomId -> PlayerMap
setPlayerLocation playerMap uid rid = M.adjust ((:) uid) rid playerMap

adjustPlayerLocation :: UserId -> RoomId -> ReaderT ThreadEnv IO ()
adjustPlayerLocation uid rid = do
    (GlobalState activeUsers w playerMap) <- readState
    case activeUsers M.!? uid of
        Nothing -> liftIO $ putStrLn "User not found"
        Just (user, _) -> 
            let rid' = userLocation user
                playerMap' = swapLocation uid rid' rid playerMap
                activeUsers' = updateActiveUsers activeUsers (userUpdateLocation user rid)
            in setState (GlobalState activeUsers' w playerMap')

getUserLocation :: ReaderT ThreadEnv IO (Either Text Room)
getUserLocation = do
    (GlobalState activeUsers w playerMap) <- readState
    uidTVar <- asks threadEnvUserId 
    mUid <- liftIO $ readTVarIO uidTVar
    case mUid of
        Nothing -> liftIO . pure $ Left "user is not logged in"
        Just uid ->
            case activeUsers M.!? uid of
                Nothing -> liftIO . pure $ Left "user is not logged in"
                Just (user, thread) ->
                    case w M.!? userLocation user of
                        Nothing -> liftIO . pure $ Left "no such room"
                        Just room -> liftIO . pure $ Right room


movePlayer :: Direction -> ReaderT ThreadEnv IO ()
movePlayer dir = do
    eRoom <- getUserLocation
    case eRoom of
        Left err -> liftIO $ print err
        Right room -> do
            let uid = roomRoomId room
            case roomAdjacent room M.!? dir of
                Nothing -> liftIO $ print "No such room"
                Just newRid -> do
                    adjustPlayerLocation uid newRid
                    showRoom

-- TODO: FIX THIS: 
showUsersInRoom :: RoomId -> ReaderT ThreadEnv IO [UserId]
showUsersInRoom rid = do
    (GlobalState activeUsers _ playerMap) <- readState
    case playerMap M.!? rid of
        Nothing -> return []
        Just uids -> return uids

showRoom :: ReaderT ThreadEnv IO ()
showRoom = do
    eRoom <- getUserLocation
    case eRoom of
        Left err -> liftIO $ print err
        Right room -> do
            uids <- showUsersInRoom $ roomRoomId room
            sendMsg $ roomName room
            sendMsg $ roomDescription room
            sendMsg . T.pack $ "You see: " ++ show uids
            sendMsg . T.pack $ "Exits: " ++ show (roomAdjacent room)
     

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
    (GlobalState activeUsers _ playerMap) <- readState
    thread <- liftIO myThreadId

    parsedUser <- liftIO $ runWordParse <$> prompt sock "Login: "
    suppressEcho
    parsedPassword <- liftIO $ runWordParse <$> prompt sock "Password: "
    unsuppressEcho

    loginResult <- liftIO $ checkLogin conn parsedUser parsedPassword
    case loginResult of
        Left err' -> liftIO (print err') >> sendMsg err' >> loginPrompt
        Right user -> do
            userIsLoggedIn (userUserId user)
            let activeUsersMap = M.insert (userUserId user) (user, thread) activeUsers
            liftIO . atomically $ writeTVar uidTVar (Just $ userUserId user)
            setState $ GlobalState activeUsersMap world playerMap
            liftIO $ print $ userUsername user +++ " Logged In"
            sendMsg "\r\nLogin Succesful"
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
                Just pass -> void . liftIO $ addUser conn (User 0 username pass 1) 

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
        Right GetUsers -> liftIO (getUsers conn) >>= sendMsg
        Right (GetUser user') -> liftIO (getUser conn user') >>= sendMsg
        Right (AddUser user') -> liftIO (addUser conn user') >>= sendMsg
        Right (Echo msg) -> sendMsg msg
        Right Exit -> logout user >> sendMsg "Goodbye!" >> liftIO (close sock)
        Right Logout -> logout user
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
    --state <- readState
    stateTVar <- asks threadEnvStateTVar
    state <- liftIO $ readTVarIO stateTVar
    thread <- liftIO myThreadId
    readTChanLoop

    liftIO . print $ globalActiveUsers state
    liftIO $ print thread
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
    state <- atomically $ newTVar (GlobalState M.empty world M.empty)
    wChannel <- newTChanIO
    let env = Env conn gameSock state wChannel
    runReaderT mainLoop env 
