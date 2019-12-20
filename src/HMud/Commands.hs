module HMud.Commands where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T

import HMud.Errors
import HMud.Room
import HMud.SqliteLib (formatAccount)
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
  GetAccounts      -> execGetUsers
  GetAccount email -> execGetUser email
  AddAccount user  -> execAddUser user
  Exit             -> execExit
  Logout           -> execLogout
  Shutdown         -> execShutdown
  Whois            -> execWhois
  Say msg          -> execSay msg
  Move dir         -> execMovePlayer dir
  Look target      -> execLook target
  Help             -> execHelp
  _                -> throwError $ InputErr InvalidCommand

execHelp :: Monad m => m Response
execHelp = pure RespHelp

execGetUsers :: (MonadReader UserEnv m , MonadDB m) => m Response
execGetUsers = do
  conn  <- asks getConnectionHandle
  accounts <- selectAllAccounts conn
  let usernames = view accountEmail <$> accounts
      newlineSeperated = T.concat $ intersperse "\n" usernames ++ pure (T.pack "\r\n")
  return $ RespAnnounce newlineSeperated

execGetUser ::
  ( MonadReader UserEnv m
  , MonadDB m
  , MonadError AppError m
  ) => Text -> m Response
execGetUser email = do
  conn <- asks getConnectionHandle
  mAccount <- selectAccount conn (T.strip email)
  case mAccount of
    Nothing -> throwError $ SessionErr EmailNotRegistered
    Just account -> return . RespAnnounce $ formatAccount account

execAddUser ::
  ( MonadReader UserEnv m
  , MonadDB m
  ) => Account -> m Response
execAddUser user = do
  conn <- asks getConnectionHandle
  Account _ username _ _ <- insertAccount conn user
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
  ) => m Response
execLogout = do
  gs <- readState
  player <- getPlayer
  let playerId = _playerPlayerId player
  setState $ gs & gsActivePlayers %~ removeUser playerId
  pure $ RespAnnounce "Logged Out"

-- TODO: Gracefully shutdown server.
execShutdown :: (MonadError AppError m) => m Response
execShutdown = return $ RespAnnounce "Shutting Down! Goodbye!"

execWhois :: ( MonadGameState m) => m Response
execWhois = RespAnnounce . whois <$> readState

execSay ::
  ( MonadPlayer m
  ) => Text -> m Response
execSay msg = do
  player <- getPlayer
  pure $ RespSay (_playerName player) msg

execMovePlayer ::
  ( MonadReader UserEnv m
  , MonadGameState m
  , MonadPlayer m
  , MonadError AppError m
  ) => Direction -> m Response
execMovePlayer dir = do
  gs <- readState
  eCurrentRoom <- getPlayerLocation
  player <- getPlayer
  let uid = _playerPlayerId player
      eUidNewRid  = (,) uid <$> destRoomId eCurrentRoom dir
  case eUidNewRid of
    Left (GameErr NoSuchRoom) -> return $ RespAnnounce "There is no path in that direction"
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
  --  room <- getPlayerlocation
  --  guard $ object `isIn` room
  --  pure . RespAnnounce $ look object
  --Dir dir -> undefined
  _ -> do
    gs <- readState
    eRoom <- getPlayerLocation
    playerId <- _playerPlayerId <$> getPlayer
    case eRoom of
      Left err -> throwError err
      Right room ->
        RespAnnounce . getRoomText . packRoomText room <$> getUsersInRoom playerId (roomRoomId room) gs



