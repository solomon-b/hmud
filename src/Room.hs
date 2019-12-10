{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Room where
-- This module will be pure functions related to Room manipulation and rendering


import Control.Monad.Reader
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

import Errors
import State
import SqliteLib
  ( User(..)
  , UserId
  )
import Parser (Direction(..))
import Types
  ( GameState(..)
  , MonadGameState(..)
  , MonadPlayer(..)
  , Room(..)
  , RoomId
  , RoomText(..)
  , UserEnv(..)
  )
import World


(+++) :: Text -> Text -> Text
(+++) = T.append


destRoomId :: Either AppError Room -> Direction -> Either AppError RoomId
destRoomId eRoom dir = do
  room <- eRoom
  maybeToEither NoSuchRoom (roomAdjacent room M.!? dir)

getUserLocation ::
  ( MonadReader UserEnv m
  , MonadGameState m
  , MonadPlayer m
  ) => m (Either AppError Room)
getUserLocation = do
  playerMap' <- globalPlayerMap <$> readState
  eUser      <- getUser
  case eUser of
    Left err -> return $ Left err
    Right user ->
      case findPlayer (userUserId user) playerMap' of
        Left err -> return $ Left err
        Right (rid, _) ->
          -- TODO: Logout of one account and into another and this blowsup:
          return . Right $ world M.! rid

getUsersInRoom :: UserId -> RoomId -> GameState -> Either AppError [User]
getUsersInRoom uid rid (GameState activeUsers' _ playerMap') = do
  uids <- maybeToEither NoSuchRoom $ playerMap' M.!? rid
  let mapFunc :: UserId -> User
      mapFunc uid' = activeUsers' M.! uid'
      filterFunc :: User -> Bool
      filterFunc (User userId _ _) = userId /= uid
      users = (filter filterFunc . fmap mapFunc) uids
  return users

showRoom' :: UserId -> Room -> GameState -> Either AppError RoomText
showRoom' uid room state =
  packRoomText room <$> getUsersInRoom uid (roomRoomId room) state

packRoomText :: Room -> [User] -> RoomText
packRoomText room players =
  let (Room name description _ exits) = room
      players' = if null players
                 then "You see no one here."
                 else "You see: " +++ T.intercalate ", " (userUsername <$> players)
      exits' = intercalate ", " $ show <$> M.keys exits
  in RoomText $ name        +++ "\n"
            +++ description +++ "\n"
            +++ players'    +++ "\n"
            +++ "You see exit(s) to the: " +++ T.pack exits'
