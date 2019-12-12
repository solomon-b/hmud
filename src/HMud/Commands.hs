module HMud.Commands where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T

import HMud.Errors
import HMud.Room
import HMud.SqliteLib (formatUser)
import HMud.State
import HMud.Types
import HMud.Types.Classes

execCommand ::
  ( MonadReader UserEnv m
  , MonadDB m
  , MonadPlayer m
  , MonadGameState m
  , MonadError AppError m
  , MonadThread m
  ) => Command -> m Response
execCommand = \case
  GetUsers         -> execGetUsers
  GetUser username -> execGetUser username
  AddUser user     -> execAddUser user
  Exit             -> execExit
  Logout           -> execLogout
  Shutdown         -> execShutdown
  Whois            -> execWhois
  Say msg          -> execSay msg
  Move dir         -> execMovePlayer dir
  Look target      -> execLook target
  Help             -> execHelp
  _                -> throwError InvalidCommand

execHelp :: Monad m => m Response
execHelp = pure RespHelp

execGetUsers :: (MonadReader UserEnv m , MonadDB m) => m Response
execGetUsers = do
  conn  <- asks getConnectionHandle
  users <- selectAllUsers conn
  let usernames = view userUsername <$> users
      newlineSeperated = T.concat $ intersperse "\n" usernames ++ pure (T.pack "\r\n")
  return $ RespAnnounce newlineSeperated

execGetUser ::
  ( MonadReader UserEnv m
  , MonadDB m
  , MonadError AppError m
  ) => Text -> m Response
execGetUser username = do
  conn <- asks getConnectionHandle
  eUser <- selectUser conn (T.strip username)
  case eUser of
    Left err' -> throwError err'
    Right user' -> return . RespAnnounce $ formatUser user'

execAddUser ::
  ( MonadReader UserEnv m
  , MonadDB m
  ) => User -> m Response
execAddUser user = do
  conn <- asks getConnectionHandle
  User _ username _ <- insertUser conn user
  return . RespAnnounce $ T.concat [username, " was added to the database"]

execExit ::
  ( MonadReader UserEnv m
  , MonadThread m
  ) => m Response
execExit = do
  socket <- asks userEnvHandle
  threadId <- getThread
  pure $ RespExit threadId socket

execLogout ::
  ( MonadReader UserEnv m
  , MonadGameState m
  , MonadPlayer m
  , MonadError AppError m
  ) => m Response
execLogout = do
  gs <- readState
  eUser <- getUser
  case eUser of
    Left err -> throwError err
    Right user -> do
      let playerId = _playerPlayerId user
      setState $ gs & gsActivePlayers %~ removeUser playerId
      return $ RespAnnounce "Logged Out"

-- TODO: Gracefully shutdown server.
execShutdown :: (MonadError AppError m) => m Response
execShutdown = return $ RespAnnounce "Shutting Down! Goodbye!"

execWhois :: ( MonadGameState m) => m Response
execWhois = RespAnnounce . whois <$> readState

execSay ::
  ( MonadPlayer m
  , MonadError AppError m
  ) => Text -> m Response
execSay msg = do
  ePlayer <- getUser
  case ePlayer of
    Left err -> throwError err
    Right user -> return $ RespSay (_playerName user) msg

execMovePlayer ::
  ( MonadReader UserEnv m
  , MonadGameState m
  , MonadPlayer m
  , MonadError AppError m
  ) => Direction -> m Response
execMovePlayer dir = do
  gs <- readState
  eCurrentRoom <- getUserLocation
  ePlayer <- getUser

  case ePlayer of
    Left err -> throwError err
    Right player -> do
      let uid = _playerPlayerId player
          eUidNewRid  = (,) uid <$> destRoomId eCurrentRoom dir
      case eUidNewRid of
        Left NoSuchRoom -> return $ RespAnnounce "There is no path in that direction"
        Left err -> throwError err
        Right (_, newRid) -> do
          setState $ gs & gsPlayerMap %~ findAndSwapPlayer uid newRid
          return $ RespAnnounce "You have moved into a new room..."

execLook ::
  ( MonadReader UserEnv m
  , MonadGameState m
  --, MonadObjectLookup m
  , MonadPlayer m
  , MonadError AppError m
  ) => Target -> m Response
execLook = \case
  --Object name -> do
  --  object <- lookupObjectByName name
  --  room <- getUserlocation
  --  guard $ object `isIn` room
  --  pure . RespAnnounce $ look object
  --Dir dir -> undefined
  _ -> do
    gs <- readState
    eRoom        <- getUserLocation
    eUser        <- getUser
    let res = do
         uid <- _playerPlayerId <$> eUser
         room <- eRoom
         showRoom' uid room gs
    case res of
      Left  err      -> throwError err
      Right roomText -> return . RespAnnounce $ getRoomText roomText
