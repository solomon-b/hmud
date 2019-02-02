{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where
    
import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.STM
import Control.Exception (Exception)
import Network.Socket.ByteString (recv, sendAll)
import Network.Socket (accept, Socket, SockAddr)
import Database.SQLite.Simple (Connection, FromRow(..), ToRow(..), field)
import Database.SQLite.Simple.Types

import TelnetLib (processStream, MessageState(..))

-------------------
---- State/Env ----
-------------------

--newtype App a = App { unApp :: ReaderT Env IO a}
--    deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO)

class HasState a where
    getState :: a -> TVar GameState
instance HasState (TVar GameState) where
    getState = id
instance HasState Env where
    getState = envStateTVar
instance HasState ThreadEnv where
    getState = threadEnvStateTVar

class HasConnection a where
    getConnection :: a -> Connection
instance HasConnection Connection where
    getConnection = id
instance HasConnection Env where
    getConnection = envConn

class HasSocket a where
    getSocket :: a -> Socket
instance HasSocket Socket where
    getSocket = id
instance HasSocket Env where
    getSocket = envSock
instance HasSocket ThreadEnv where
    getSocket = threadEnvSock

class Monad m => MonadThread m where
    getThread :: m ThreadId
instance MonadIO m => MonadThread (ReaderT env m) where
    getThread = liftIO myThreadId

class Monad m => MonadMessaging m where
    duplicateChannel :: TChan a -> m (TChan a) 
    writeChannel     :: TChan a -> a -> m ()
    readChannel      :: TChan a -> m a
instance MonadIO m => MonadMessaging (ReaderT env m) where
    duplicateChannel   = liftIO . atomically . dupTChan
    writeChannel tchan = liftIO . atomically . writeTChan tchan
    readChannel        = liftIO . atomically . readTChan

class Monad m => MonadTCP m where
    acceptSocket :: Socket -> m (Socket, SockAddr)
    readSocket   :: Socket -> m ByteString
    sendSocket   :: Socket -> ByteString -> m ()
instance (HasSocket env, MonadIO m) => MonadTCP (ReaderT env m) where
    acceptSocket =  liftIO    . accept 
    readSocket   =  liftIO    . flip recv 1024
    sendSocket   = (liftIO .) . sendAll

class MonadTCP m => MonadPrompt m where
    prompt :: ByteString -> m ByteString
instance (HasSocket env, MonadIO m) => MonadPrompt (ReaderT env m) where
    prompt prefix = do
        sock <- asks getSocket
        sendSocket sock (BS.append prefix (BS.pack [255, 249]))
        rawMsg <- readSocket sock
        let (MessageState msg _) = processStream rawMsg
        case msg of
            Nothing   -> prompt ""
            Just msg' -> return msg'

class Monad m => MonadGameState m where
    modifyState' :: (GameState -> GameState) -> m ()
    setState'    :: GameState -> m ()
    readState'   :: m GameState
instance (HasState env, MonadIO m) => MonadGameState (ReaderT env m) where
    modifyState' f = asks getState >>= (liftIO . atomically . flip modifyTVar' f)
    setState' s    = asks getState >>= (liftIO . atomically . flip writeTVar s)
    readState'     = asks getState >>= (liftIO . atomically . readTVar)

data Env = 
    Env { envConn      :: Connection
        , envSock      :: Socket
        , envStateTVar :: TVar GameState
        , envWChannel  :: TChan Msg
        -- TODO: Figure out why this isnt in a TVar
        , envUsers     :: [User]
        } 

data ThreadEnv =
    ThreadEnv { threadEnvConn      :: Connection
              , threadEnvSock      :: Socket
              , threadEnvStateTVar :: TVar GameState
              , threadEnvWChannel  :: TChan Msg
              , threadEnvRChannel  :: TChan Msg
              , threadEnvUserId    :: TVar (Maybe UserId)
              -- TODO: Figure out why this isnt in a TVar
              , threadEnvUsers     :: [User]        
              }

type Msg = Text
type Username = Text
type ActiveUsers = Map UserId (User, ThreadId)

data GameState = 
    GameState { globalActiveUsers :: ActiveUsers
              , globalWorld       :: World
              , globalPlayerMap   :: PlayerMap
              } deriving Show


----------------
---- Errors ----
----------------

data AppError = NoSuchUser
           | NoSuchRoom
           | NotLoggedIn
           | AlreadyLoggedIn
           | UserNotFound
           | InvalidPassword
           | PasswordsDontMatch
           | UserNotInPlayerMap
           | InvalidCommand
           | BadParse String
           deriving Show


-------------------
---- The World ----
-------------------

data Direction = 
    N | S | E | W | NW | NE | SW | SE | U | D deriving (Eq, Ord)

instance Show Direction where
    show U = "Up"
    show D = "Down"
    show N = "North"
    show S = "South"
    show E = "East"
    show W = "West"
    show NW = "Northwest"
    show NE = "Northeast"
    show SW = "Southwest"
    show SE = "Southeast"

type Name = Text
type Description = Text
type RoomId = Integer
type World = Map RoomId Room
type PlayerMap = Map RoomId [UserId]

data Room = 
    Room { roomName        :: Name
         , roomDescription :: Description
         , roomRoomId      :: RoomId
         , roomAdjacent    :: Map Direction RoomId
         }

instance Show Room where
    show (Room name desc _ dir) =
        show name ++ "\n" ++
        show desc ++ "\n" ++
        "Exits: " ++ show (M.keys dir)

newtype RoomText = RoomText { getRoomText :: Text } deriving Show


----------------
---- Parser ----
----------------

data Command = 
      GetUsers
    | GetUser Text
    | AddUser User
    | Echo Text
    | Exit
    | Shutdown
    | Register
    | Look
    | Login
    | Logout
    | Whois
    | Say Text
    | Move Direction
    deriving (Eq, Show)


------------------
---- Database ----
------------------
type UserId = Integer

data User =
    User { userUserId   :: Integer
         , userUsername :: Text
         , userPassword :: Text
         } deriving Eq

type UserRow = (Null, Text, Text)

data DuplicateData = DuplicateData 
    deriving (Eq, Show, Typeable)

instance Exception DuplicateData

instance FromRow User where
    fromRow = User <$> field <*> field <*> field

instance ToRow User where
    toRow (User id_ username' password') = toRow (id_, username', password')

instance Show User where
    show user = show (userUsername user)
