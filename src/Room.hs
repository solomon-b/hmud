{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Room where
-- This module will be pure functions related to Room manipulation and rendering


import Control.Concurrent.STM
import qualified Control.Monad.Reader as R
import Control.Monad.IO.Class (liftIO)
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
    , MonadThread(..)
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
    ( R.MonadReader UserEnv m
    , R.MonadIO m
    , MonadGameState m
    , MonadThread m
    ) => m (Either AppError Room) 
getUserLocation = do
    playerMap' <- globalPlayerMap <$> readState
    tvarUid    <- R.asks userEnvUserId --getUserId
    mUid       <- liftIO . atomically $ readTVar tvarUid
    let eUid = maybe (Left NoSuchUser) Right mUid
    case eUid >>= flip findPlayer playerMap' of
        Left err -> liftIO . pure $ Left err
        Right (rid, _) -> do 
            liftIO . putStrLn $ "user is in room: " ++ show rid
            -- TODO: Logout of one account and into another and this blowsup:
            liftIO . pure . Right $ world M.! rid

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

--showRoom ::
--    ( R.MonadReader UserEnv m
--    , R.MonadIO m
--    , MonadGameState m
--    , MonadThread m
--    , MonadTCP m
--    ) => m ()
--showRoom = do
--    globalState' <- readState
--    tvarUid      <- R.asks userEnvUserId --getUserId
--    mUid         <- liftIO . atomically $ readTVar tvarUid
--    let eUid = maybe (Left NoSuchUser) Right mUid
--    eRoom        <- getUserLocation
--    let res = do
--            uid  <- eUid
--            room <- eRoom
--            showRoom' uid room globalState'
--    case res of
--        Left err       -> liftIO  $ print err
--        Right roomText -> sendMsg $ getRoomText roomText

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
