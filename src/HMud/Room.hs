module HMud.Room where
-- This module will be pure functions related to Room manipulation and rendering


import Control.Lens
import Control.Monad.Reader
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T


import HMud.Errors
import HMud.State
import HMud.Types
import HMud.Types.Classes
import HMud.World


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
  gs <- readState
  ePlayer <- getUser
  case ePlayer of
    Left err -> return $ Left err
    Right player ->
      case findPlayer (player ^. playerPlayerId) (gs ^. gsPlayerMap) of
        Left err -> return $ Left err
        Right (rid, _) ->
          return . Right $ world M.! rid

getUsersInRoom :: PlayerId -> RoomId -> GameState -> Either AppError [Player]
getUsersInRoom uid rid gs = do
  uids <- gs ^. gsPlayerMap . at rid . to (maybe (Left NoSuchRoom) Right)
  let players :: [Maybe Player]
      players = fmap (\playerId -> gs ^. gsActivePlayers . at playerId) uids
  pure $ filter (\player -> player ^. playerPlayerId == uid) (players ^.. traversed . traversed)

showRoom' :: PlayerId -> Room -> GameState -> Either AppError RoomText
showRoom' uid room state =
  packRoomText room <$> getUsersInRoom uid (roomRoomId room) state

packRoomText :: Room -> [Player] -> RoomText
packRoomText room players =
  let (Room name description _ exits) = room
      players' = if null players
                 then "You see no one here."
                 else "You see: " +++ T.intercalate ", " (_playerName <$> players)
      exits' = intercalate ", " $ show <$> M.keys exits
  in RoomText $ name        +++ "\n"
            +++ description +++ "\n"
            +++ players'    +++ "\n"
            +++ "You see exit(s) to the: " +++ T.pack exits'
