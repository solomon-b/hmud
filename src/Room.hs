{-# LANGUAGE OverloadedStrings #-}
module Room where
-- This module will be pure functions related to Room manipulation and rendering


import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

import Dispatch
import State
import Types ( Error(..)
             , Direction
             , GlobalState(..)
             , Room(..)
             , RoomId
             , RoomText(..)
             , ThreadEnv(..)
             , User(..)
             , UserId
             )
import World


(+++) :: Text -> Text -> Text
(+++) = T.append


destRoomId :: Either Error Room -> Direction -> Either Error RoomId
destRoomId eRoom dir = do
    room <- eRoom
    maybeToEither NoSuchRoom (roomAdjacent room M.!? dir)

getUserLocation :: ReaderT ThreadEnv IO (Either Error Room)
getUserLocation = do
    playerMap' <- globalPlayerMap <$> readState
    eUid <- getUserId 
    case eUid >>= flip findPlayer playerMap' of
        Left err -> liftIO . pure $ Left err
        Right (rid, _) -> do 
            liftIO . putStrLn $ "user is in room: " ++ show rid
            liftIO . pure . Right $ world M.! rid

spawnPlayer :: ReaderT ThreadEnv IO ()
spawnPlayer = do
    liftIO $ putStrLn "Spawning Player.."
    (GlobalState activeUsers' w playerMap') <- readState
    eUid <- getUserId 
    case eUid of
        Left err -> liftIO $ print err
        Right uid -> do
            let playerMap'' = addPlayer uid 1 playerMap'
            setState (GlobalState activeUsers' w playerMap'')
            liftIO $ putStrLn "..Player spawned"

getUsersInRoom :: UserId -> RoomId -> GlobalState -> Either Error [User]
getUsersInRoom uid rid (GlobalState activeUsers' _ playerMap') = do
    uids <- maybeToEither NoSuchRoom $ playerMap' M.!? rid
    let mapFunc :: UserId -> User
        mapFunc uid' = fst $ activeUsers' M.! uid'
        filterFunc :: User -> Bool
        filterFunc (User userId _ _) = userId /= uid
        users = (filter filterFunc . fmap mapFunc) uids
    return users

showRoom' :: UserId -> Room -> GlobalState -> Either Error RoomText
showRoom' uid room state =
    packRoomText room <$> getUsersInRoom uid (roomRoomId room) state

showRoom :: ReaderT ThreadEnv IO ()
showRoom = do
    globalState' <- readState
    eUid <- getUserId
    eRoom <- getUserLocation
    let res = do
            uid <- eUid
            room <- eRoom
            showRoom' uid room globalState'
    case res of
        Left err -> liftIO $ print err
        Right roomText -> sendMsg $ getRoomText roomText

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
