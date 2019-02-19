{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where
    
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T (concat, pack)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.STM

import Errors
import Parser (Command, Direction)
import qualified Socket 
import qualified SqliteLib as SQL
import SqliteLib (User, UserId)
import TelnetLib (processStream)

---------------------
---- MTL Classes ----
---------------------

class HasState env where
    getState :: env -> TVar GameState
instance HasState (TVar GameState) where
    getState = id
instance HasState Env where
    getState = envStateTVar
instance HasState UserEnv where
    getState = userEnvStateTVar

class HasConnectionHandle env where
    getConnectionHandle :: env -> SQL.Handle
instance HasConnectionHandle SQL.Handle where
    getConnectionHandle = id
instance HasConnectionHandle Env where
    getConnectionHandle = envConnHandle
instance HasConnectionHandle UserEnv where
    getConnectionHandle = userEnvConnHandle

class HasSocketHandle env where
    getSocketHandle :: env -> Socket.Handle
instance HasSocketHandle Socket.Handle where
    getSocketHandle = id
instance HasSocketHandle Env where
    getSocketHandle = envHandle
instance HasSocketHandle UserEnv where
    getSocketHandle = userEnvHandle

class HasUserId env where
    getUserId :: env -> TVar (Maybe UserId)
instance HasUserId (TVar (Maybe UserId)) where
    getUserId = id
instance HasUserId UserEnv where
    getUserId = userEnvUserId

class Monad m => MonadThread m where
    getThread :: m ThreadId
instance MonadIO m => MonadThread (ReaderT env m) where
    getThread = liftIO myThreadId
instance MonadIO m => MonadThread (ExceptT e m) where
    getThread = liftIO myThreadId

class Monad m => MonadTChan m where
    createTChan      :: m (TChan a)
    duplicateChannel :: TChan a -> m (TChan a) 
    writeChannel     :: TChan a -> a -> m ()
    readChannel      :: TChan a -> m a
instance MonadIO m => MonadTChan (ReaderT env m) where
    createTChan        = liftIO . atomically $ newTChan
    duplicateChannel   = liftIO . atomically . dupTChan
    writeChannel tchan = liftIO . atomically . writeTChan tchan
    readChannel        = liftIO . atomically . readTChan
instance MonadIO m => MonadTChan (StateT s m) where
    createTChan        = liftIO . atomically $ newTChan
    duplicateChannel   = liftIO . atomically . dupTChan
    writeChannel tchan = liftIO . atomically . writeTChan tchan
    readChannel        = liftIO . atomically . readTChan
instance MonadIO m => MonadTChan (ExceptT e m) where
    createTChan        = liftIO . atomically $ newTChan
    duplicateChannel   = liftIO . atomically . dupTChan
    writeChannel tchan = liftIO . atomically . writeTChan tchan
    readChannel        = liftIO . atomically . readTChan
instance MonadTChan IO where
    createTChan        = atomically   newTChan
    duplicateChannel   = atomically . dupTChan
    writeChannel tchan = atomically . writeTChan tchan
    readChannel        = atomically . readTChan

class Monad m => MonadTCP m where
    acceptHandle' :: Socket.Handle -> m Socket.Handle
    readHandle'   :: Socket.Handle -> m ByteString
    sendHandle'   :: Socket.Handle -> ByteString -> m ()
    --closeHandle'  :: Socket.Handle -> m ()
instance MonadIO m => MonadTCP (ReaderT env m) where
    acceptHandle' =  liftIO    . Socket.acceptHandle 
    readHandle'   =  liftIO    . Socket.readHandle
    sendHandle'   = (liftIO .) . Socket.sendHandle
instance MonadIO m => MonadTCP (StateT s m) where
    acceptHandle' =  liftIO    . Socket.acceptHandle 
    readHandle'   =  liftIO    . Socket.readHandle
    sendHandle'   = (liftIO .) . Socket.sendHandle
instance MonadTCP IO where
    acceptHandle' = Socket.acceptHandle 
    readHandle'   = Socket.readHandle
    sendHandle'   = Socket.sendHandle

class Monad m => MonadDB m where
    insertUser     :: SQL.Handle -> User -> m User
    selectUser     :: SQL.Handle -> Text -> m (Either AppError User)
    selectAllUsers :: SQL.Handle -> m [User]
instance MonadIO m => MonadDB (ReaderT env m) where
    insertUser handle user  = liftIO $ SQL.insertUser handle user
    selectUser handle text  = liftIO $ SQL.selectUser handle text
    selectAllUsers          = liftIO . SQL.selectAllUsers

class MonadTCP m => MonadPrompt m where
    prompt :: ByteString -> m ByteString
instance (HasSocketHandle env, MonadIO m) => MonadPrompt (ReaderT env m) where
    prompt prefix = do
        handle <- asks getSocketHandle
        sendHandle' handle (BS.append prefix (BS.pack [255, 249]))
        rawMsg <- readHandle' handle
        case processStream rawMsg of
            Left _   -> prompt ""
            Right msg -> return msg

class Monad m => MonadGameState m where
    modifyState :: (GameState -> GameState) -> m ()
    setState    :: GameState -> m ()
    readState   :: m GameState
instance (HasState env, MonadIO m) => MonadGameState (ReaderT env m) where
    modifyState f = asks getState >>= (liftIO . atomically . flip modifyTVar' f)
    setState    s = asks getState >>= (liftIO . atomically . flip writeTVar s)
    readState     = asks getState >>= (liftIO . atomically . readTVar)

class Monad m => MonadPlayer m where
    getUser :: m (Either AppError User)
    setUser :: UserId -> m ()
instance ( HasUserId env
         , HasState env
         , MonadIO m
         ) => MonadPlayer (ReaderT env m) where
    getUser = do
        (GameState activePlayers _ _) <- readState
        tvar <- asks getUserId
        mUid <- liftIO . atomically $ readTVar tvar
        case mUid of
            Nothing -> return $ Left NotLoggedIn
            Just uid -> 
                let user = M.lookup uid activePlayers
                in return $ maybe (Left NoSuchUser) Right user
    setUser uid = do
        tvar <- asks getUserId
        liftIO . atomically $ writeTVar tvar (Just uid)


-------------------
---- State/Env ----
-------------------

data Env = 
    Env { envStateTVar  :: TVar GameState
        , envWChannel   :: TChan Response
        , envConnHandle :: SQL.Handle
        , envHandle     :: Socket.Handle
        } 

data UserEnv =
    UserEnv { userEnvConnHandle :: SQL.Handle              -- Remove Soon?
            , userEnvHandle     :: Socket.Handle           -- Remove Soon?
            , userEnvStateTVar  :: TVar GameState          -- Shared State
            , userEnvPubTChan   :: TChan Response          -- Public Message Channel
            --, userEnvCmdTChan   :: TChan Command           -- Read Commands from the socket
            , userEnvCmdTChan   :: TChan (Either AppError Command) -- Read Commands from the socket
            , userEnvRespTchan  :: TChan Response          -- Write Responses to the socket
            , userEnvUserId     :: TVar (Maybe UserId) -- Current User ID
            }

type ActiveUsers = Map UserId User
data GameState = 
    GameState { globalActiveUsers :: ActiveUsers
              , globalWorld       :: World
              , globalPlayerMap   :: PlayerMap
              } deriving Show


------------------------
---- Response Types ----
------------------------

data Response 
    = RespSay Username Msg
    | RespLook Text
    | RespAnnounce Text
    | Prompt Text
    | RespShutdown
    | RespExit
    | RespAppError AppError

instance Show Response where
    show (RespSay user msg)  = concat ["<", show user, "> ", show msg]
    show (RespLook text)     = show text
    show (RespAnnounce text) = show text
    show (Prompt text)   = show text
    show RespShutdown        = "RespShutdown"
    show RespExit            = "RespExit"
    show (RespAppError err)  = show err

instance TShow Response where
    tshow (RespSay username msg) = T.concat ["<", username, "> ", msg, "\r\n"]
    tshow (Prompt text)      = text
    tshow resp                   = T.pack $ show resp ++ "\r\n"

type Msg = Text
type Username = Text

class TShow a where
    tshow :: a -> Text

-------------------
---- The World ----
-------------------

type Name = Text

type Description = Text
type RoomId      = Integer
type World       = Map RoomId Room
type PlayerMap   = Map RoomId [UserId]


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
instance TShow Room where
    tshow = T.pack . show

newtype RoomText = RoomText { getRoomText :: Text } deriving Show
