module HMud.Types.Classes where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.STM
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens

import Data.ByteString (ByteString)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T

import HMud.Errors
import HMud.Types
import qualified HMud.Socket as Socket
import qualified HMud.SqliteLib as SQL
import HMud.SqliteLib (HasConnectionHandle(..))
import HMud.SqliteLib (Account(..), AccountId)


--------------------
--- Environments ---
--------------------

data Env =
  Env { envStateTVar  :: TVar GameState
      , envWChannel   :: TChan Response
      , envConnHandle :: SQL.Handle
      , envHandle     :: Socket.Handle
      }

instance HasConnectionHandle Env where
  getConnectionHandle = envConnHandle

data UserEnv =
  UserEnv { userEnvConnHandle :: SQL.Handle              -- Remove Soon?
          , userEnvHandle     :: Socket.Handle           -- Remove Soon?
          , userEnvStateTVar  :: TVar GameState          -- Shared State
          , userEnvPubTChan   :: TChan Response          -- Public Message Channel
          --, userEnvCmdTChan   :: TChan Command           -- Read Commands from the socket
          , userEnvCmdTChan   :: TChan (Either AppError Command) -- Read Commands from the socket
          , userEnvRespTchan  :: TChan Response          -- Write Responses to the socket
          , userEnvAccountId     :: TVar (Maybe AccountId) -- Current Account ID
          }

instance HasConnectionHandle UserEnv where
  getConnectionHandle = userEnvConnHandle


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

class HasSocketHandle env where
  getSocketHandle :: env -> Socket.Handle
instance HasSocketHandle Socket.Handle where
  getSocketHandle = id
instance HasSocketHandle Env where
  getSocketHandle = envHandle
instance HasSocketHandle UserEnv where
  getSocketHandle = userEnvHandle

class HasAccountId env where
  getAccountIdTVar :: env -> TVar (Maybe AccountId)
instance HasAccountId (TVar (Maybe AccountId)) where
  getAccountIdTVar = id
instance HasAccountId UserEnv where
  getAccountIdTVar = userEnvAccountId

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
  insertAccount     :: SQL.Handle -> Account -> m Account
  selectAccount     :: SQL.Handle -> Text -> m (Maybe Account)
  selectAllAccounts :: SQL.Handle -> m [Account]
  insertPlayer   :: SQL.Handle -> Player -> m Player
instance MonadIO m => MonadDB (ReaderT env m) where
  insertAccount handle user     = liftIO $ SQL.insertAccount handle user
  selectAccount handle text     = liftIO $ SQL.selectAccount handle text
  selectAllAccounts             = liftIO . SQL.selectAllAccounts
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

class Monad m => MonadPlayer m where
  getAccountId :: m AccountId
  setAccountId :: AccountId -> m ()
  getPlayer :: m Player
  createPlayer :: AccountId -> Text -> Text -> m Response
  login :: Account -> m Response
  logout :: Account -> m ()
  throwIfAccountInActiveSession :: Account -> m ()
  throwIfThreadInActiveSession :: m ()
  isActiveSession :: m Bool

instance
  ( HasAccountId env
  , HasState env
  , HasConnectionHandle env
  , MonadError AppError m
  , MonadIO m
  ) => MonadPlayer (ReaderT env m) where
  getAccountId = do
    tvar <- asks getAccountIdTVar
    mAccountId <- liftIO . atomically $ readTVar tvar
    maybe (throwError $ SessionErr NoActiveSession) pure mAccountId
  setAccountId uid = do
    tvar <- asks getAccountIdTVar
    liftIO . atomically $ writeTVar tvar (Just uid)
  getPlayer = do
    conn <- asks getConnectionHandle
    accId <- getAccountId
    sqlResp <- liftIO $ SQL.selectPlayerByAccountId conn accId
    case sqlResp of
      Just player -> pure player
      Nothing -> throwError $ SessionErr NoAccountPlayer
  createPlayer uid name desc = do
    handle <- asks getConnectionHandle
    void . insertPlayer handle $ Player (PlayerId 0) uid name desc (InventoryId 0)
    pure $ RespPlayerCreated name
  login account = do
    let uid = account ^. accountId
    -- Set accountId in UserEnv
    setAccountId uid
    -- Add AccountId to ActiveAccounts List
    modifyState $ gsActiveAccounts %~ (:) uid
    -- Get the Player associated with the account
    player <- getPlayer
    -- Get the Player ID
    let playerId = (player ^. playerPlayerId)
    -- Add the Player to the ActivePlayers Map
    modifyState $ gsActivePlayers . at playerId ?~ player
    -- Add the Player to the world in room 1
    modifyState $ gsPlayerMap . at (RoomId 1) . non mempty <>~ pure playerId
    pure . RespAnnounce $ _accountEmail account `T.append` " Logged In"
  logout = error "Unimplemented"
  throwIfAccountInActiveSession account = do
    let idx = account ^. accountId
    accountIds <- _gsActiveAccounts <$> readState
    maybe (pure ()) (const $ throwError $ SessionErr AccountAlreadyActive) (find (== idx) accountIds)
  throwIfThreadInActiveSession = do
    tvar <- asks getAccountIdTVar
    mAccountId <- liftIO . atomically $ readTVar tvar
    maybe (pure ()) (const $ throwError $ SessionErr ActiveSession) mAccountId
  isActiveSession = do
    env <- ask
    player <- runExceptT $ runReaderT getPlayer env
    either (const $ pure False) (const $ pure True) player

class MonadGameState m => MonadObjectLookup m where
  lookupObjectByName :: Text -> m Item
