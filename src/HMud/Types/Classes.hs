module HMud.Types.Classes where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.STM
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Map.Strict as M

import HMud.Errors
import HMud.Types
import qualified HMud.Socket as Socket
import qualified HMud.SqliteLib as SQL
import HMud.SqliteLib (User(..), UserId)

--------------------
--- Environments ---
--------------------

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
  getUserIdTVar :: env -> TVar (Maybe UserId)
instance HasUserId (TVar (Maybe UserId)) where
  getUserIdTVar = id
instance HasUserId UserEnv where
  getUserIdTVar = userEnvUserId

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
  closeHandle'  :: Socket.Handle -> m ()
instance MonadIO m => MonadTCP (ReaderT env m) where
  acceptHandle' =  liftIO    . Socket.acceptHandle
  readHandle'   =  liftIO    . Socket.readHandle
  sendHandle'   = (liftIO .) . Socket.sendHandle
  closeHandle'  = liftIO     . Socket.closeHandle
instance MonadIO m => MonadTCP (StateT s m) where
  acceptHandle' =  liftIO    . Socket.acceptHandle
  readHandle'   =  liftIO    . Socket.readHandle
  sendHandle'   = (liftIO .) . Socket.sendHandle
  closeHandle'  = liftIO     . Socket.closeHandle
instance MonadTCP IO where
  acceptHandle' = Socket.acceptHandle
  readHandle'   = Socket.readHandle
  sendHandle'   = Socket.sendHandle
  closeHandle'  = Socket.closeHandle

class Monad m => MonadDB m where
  insertUser     :: SQL.Handle -> User -> m User
  selectUser     :: SQL.Handle -> Text -> m (Either AppError User)
  selectAllUsers :: SQL.Handle -> m [User]
  insertPlayer   :: SQL.Handle -> Player -> m Player
instance MonadIO m => MonadDB (ReaderT env m) where
  insertUser handle user     = liftIO $ SQL.insertUser handle user
  selectUser handle text     = liftIO $ SQL.selectUser handle text
  selectAllUsers             = liftIO . SQL.selectAllUsers
  insertPlayer handle player = liftIO $ SQL.insertPlayer handle player

--class MonadTCP m => MonadPrompt m where
--  prompt :: ByteString -> m ByteString
---- NOTE: This instance does not behave properly and should not be used without a rewrite.
--instance (HasSocketHandle env, MonadIO m) => MonadPrompt (ReaderT env m) where
--  prompt prefix = do
--    handle <- asks getSocketHandle
--    sendHandle' handle (BS.append prefix (BS.pack [255, 249]))
--    rawMsg <- readHandle' handle
--    case (unBuffer . processStream) rawMsg of
--      Nothing  -> prompt prefix
--      Just msg -> return msg

class Monad m => MonadGameState m where
  modifyState :: (GameState -> GameState) -> m ()
  setState    :: GameState -> m ()
  readState   :: m GameState
instance (HasState env, MonadIO m) => MonadGameState (ReaderT env m) where
  modifyState f = asks getState >>= (liftIO . atomically . flip modifyTVar' f)
  setState    s = asks getState >>= (liftIO . atomically . flip writeTVar s)
  readState     = asks getState >>= (liftIO . atomically . readTVar)

userIdToPlayer :: GameState -> UserId -> Maybe Player
userIdToPlayer gs uid = do
  playerId <- M.lookup uid $ _gsUserMap gs
  player <- M.lookup playerId $ _gsActivePlayers gs
  return player

class Monad m => MonadPlayer m where
  getUserId :: m (Maybe UserId)
  getUser :: m (Either AppError Player)
  setUser :: UserId -> m ()
instance ( HasUserId env , HasState env , MonadIO m) => MonadPlayer (ReaderT env m) where
  getUserId = do
    tvar <- asks getUserIdTVar
    liftIO . atomically $ readTVar tvar
  getUser = do
    gs <- readState
    tvar <- asks getUserIdTVar
    mUid <- liftIO . atomically $ readTVar tvar
    case mUid of
      Nothing -> return $ Left NotLoggedIn
      Just uid -> return $ maybe (Left NoSuchUser) Right $ userIdToPlayer gs uid
  setUser uid = do
    tvar <- asks getUserIdTVar
    liftIO . atomically $ writeTVar tvar (Just uid)

class MonadGameState m => MonadObjectLookup m where
  lookupObjectByName :: Text -> m Item
