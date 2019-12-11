module HMud.Commands where

import Control.Monad.Except
import Control.Monad.Reader
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T

import HMud.Errors
import HMud.Room
import HMud.Parser.Commands
import HMud.SqliteLib (User(..), formatUser)
import HMud.State
import HMud.Types
  ( GameState(..)
  , HasConnectionHandle(..)
  , MonadDB(..)
  , MonadGameState(..)
  , MonadPlayer(..)
  , MonadThread(..)
  , RoomText(..)
  , Response(..)
  , UserEnv(..)
  )
import HMud.World

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
  let usernames = userUsername <$> users
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
  (GameState activePlayers _ playerMap') <- readState
  eUser <- getUser
  case eUser of
    Left err -> throwError err
    Right user -> do
      let userId = userUserId user
          activePlayers' = removeUser userId activePlayers
      setState $ GameState activePlayers' world playerMap'
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
  eUser <- getUser
  case eUser of
    Left err -> throwError err
    Right user -> return $ RespSay (userUsername user) msg

execMovePlayer ::
  ( MonadReader UserEnv m
  , MonadGameState m
  , MonadPlayer m
  , MonadError AppError m
  ) => Direction -> m Response
execMovePlayer dir = do
  state <- readState
  eCurrentRoom <- getUserLocation
  eUser <- getUser

  case eUser of
    Left err -> throwError err
    Right (User uid _ _) -> do
      let eUidNewRid  = (,) <$> pure uid <*> destRoomId eCurrentRoom dir
      case eUidNewRid of
        Left NoSuchRoom -> return $ RespAnnounce "There is no path in that direction"
        Left err -> throwError err
        Right (_, newRid) -> do
          let playerMap' = findAndSwapPlayer uid newRid (globalPlayerMap state)
          setState $ replacePlayerMap state playerMap'
          return $ RespAnnounce "You have moved into a new room..."

execLook ::
  ( MonadReader UserEnv m
  , MonadGameState m
  , MonadPlayer m
  , MonadError AppError m
  ) => Target -> m Response
execLook target = do
  globalState' <- readState
  eRoom        <- getUserLocation
  eUser        <- getUser
  let res = do
       uid <- userUserId <$> eUser
       room <- eRoom
       showRoom' uid room globalState'
  case res of
    Left  err      -> throwError err
    Right roomText -> return . RespAnnounce $ getRoomText roomText
