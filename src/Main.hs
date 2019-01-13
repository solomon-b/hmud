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
import Types (Command(..), Env(..), GlobalState(..), ThreadEnv(..), User(..))
import Parser


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

addUser :: Connection -> User -> IO Text
addUser conn (User _ username password) = do
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
    stateMap <- liftIO $ globalActiveUsers <$> readTVarIO stateTVar
    writeTVarR $ GlobalState $ M.delete username stateMap
    where username = userUsername currUser

readState :: ReaderT ThreadEnv IO GlobalState
readState = do
    stateTVar <- asks threadEnvStateTVar
    liftIO $ readTVarIO stateTVar

writeTVarR :: GlobalState -> ReaderT ThreadEnv IO ()
writeTVarR state = do
    stateTVar <- asks threadEnvStateTVar
    liftIO . atomically $ writeTVar stateTVar state

broadcast :: String -> ReaderT ThreadEnv IO ()
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
    sendMsg (T.pack msg)

-- TODO: Cleanup this function:
whois :: GlobalState -> Text
whois curState = T.pack . intercalate ", " . fmap (show . fst) $ M.elems $ globalActiveUsers curState

userIsLoggedIn :: Text -> ReaderT ThreadEnv IO ()
userIsLoggedIn username = do
    curState <- readState
    let stateMap = globalActiveUsers curState
        mUser = M.lookup username stateMap
    return ()
    

-----------------
---- Prompts ----
-----------------

mainMenu :: ReaderT ThreadEnv IO ()
mainMenu = undefined

loginPrompt :: ReaderT ThreadEnv IO ()
loginPrompt = do
    conn <- asks threadEnvConn
    sock <- asks threadEnvSock
    curState <- readState
    thread <- liftIO myThreadId

    user <- liftIO $ prompt sock "Login: "
    suppressEcho
    password <- liftIO $ prompt sock "Password: "
    unsuppressEcho
    let parsedUser = resultToEither $ parseByteString word mempty user
    let parsedPassword = resultToEither $ parseByteString word mempty password
    
    loginResult <- liftIO $ checkLogin conn parsedUser parsedPassword
    case loginResult of
        Left err' -> liftIO (print err') >> sendMsg err' >> loginPrompt
        Right user' -> do
            let stateMap = globalActiveUsers curState
            writeTVarR . GlobalState $ M.insert (userUsername user') (user', thread) stateMap
            liftIO $ print $ userUsername user' +++ " Logged In"
            sendMsg "\r\nLogin Succesful"
            userLoop

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
                Just pass -> void . liftIO $ addUser conn (User 0 username pass) 


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
        Right (Say msg) -> broadcast . T.unpack $ T.concat ["<", userUsername user, "> ", msg]
        Left err' -> sendMsg "Command not recognized" >> liftIO (print err')


--------------
---- Main ----
--------------

userLoop :: ReaderT ThreadEnv IO ()
userLoop = do
    stateTVar <- asks threadEnvStateTVar

    state <- liftIO $ readTVarIO stateTVar
    thread <- liftIO myThreadId
    readTChanLoop
    liftIO $ print state
    liftIO $ print thread
    let user = find (\(_, tid) -> tid == thread) $ globalActiveUsers state

    gamePrompt user 
    userLoop 
    
mainLoop :: ReaderT Env IO ()
mainLoop = forever $ do
    stateTVar <- asks envStateTVar
    conn <- asks envConn
    sock <- asks envSock
    wChannel<- asks envWChannel
    rChannel <- liftIO . atomically $ dupTChan wChannel
    (sock', _) <- lift $ accept sock

    void . forkReader . forever . liftIO . atomically $ readTChan rChannel

    lift $ do
        putStrLn "Got connection, handling query"
        let threadEnv = ThreadEnv conn sock' stateTVar wChannel rChannel
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
    state <- atomically $ newTVar (GlobalState M.empty)
    wChannel <- newTChanIO
    let env = Env conn gameSock state wChannel

    runReaderT mainLoop env 
