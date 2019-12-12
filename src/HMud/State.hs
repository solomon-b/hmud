module HMud.State where

import qualified Data.Map.Strict as M
import Data.List (intersperse, find)
import Data.Text (Text)
import qualified Data.Text as T

import HMud.Errors
import HMud.Types

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing = Left a

----------------------------------
---- GameState Manipulation ----
----------------------------------

--- ActivePlayers ---

removeUser :: PlayerId -> M.Map PlayerId Player -> M.Map PlayerId Player
removeUser = M.delete

addUser :: Player -> ActivePlayers -> ActivePlayers
addUser user = M.insert (_playerPlayerId user) user

whois :: GameState -> Text
whois state =
 let players = M.elems $ _gsActivePlayers state
     names = _playerName <$> players
 in T.concat . intersperse (T.pack "\n") $ names

--- PlayerMap ---

replacePlayerMap :: GameState -> PlayerMap -> GameState
replacePlayerMap gs newPlayerMap = gs { _gsPlayerMap = newPlayerMap}

removePlayer :: PlayerId -> RoomId -> PlayerMap -> PlayerMap
removePlayer uid =
  M.adjust (filter (/= uid))

addPlayer :: PlayerId -> RoomId -> PlayerMap -> PlayerMap
addPlayer uid =
  M.adjust ((:) uid)

lookupPlayer :: PlayerId -> ActivePlayers -> Either AppError Player
lookupPlayer uid activePlayers =
  let player = M.lookup uid activePlayers
  in maybe (Left NoSuchUser) Right player

swapPlayer :: PlayerId -> RoomId -> RoomId -> PlayerMap -> PlayerMap
swapPlayer uid rid rid' =
  addPlayer uid rid' . removePlayer uid rid

findPlayer :: PlayerId -> M.Map RoomId [PlayerId] -> Either AppError (RoomId, PlayerId)
findPlayer uid playerMap' =
  let f :: (RoomId, [PlayerId]) -> [(RoomId, PlayerId)]
      f (i, xs) = (,) i <$> xs
      players :: [(RoomId, PlayerId)]
      players = concatMap f (M.toList playerMap')
      g :: (RoomId, PlayerId) -> Bool
      g (_, uid') = uid == uid'
      player :: Maybe (RoomId, PlayerId)
      player = find g players
  in maybeToEither UserNotInPlayerMap player

findAndSwapPlayer :: PlayerId -> RoomId -> PlayerMap -> PlayerMap
findAndSwapPlayer uid rid playerMap' =
  case findPlayer uid playerMap' of
    Right (rid', _) -> swapPlayer uid rid' rid playerMap'
    _ -> addPlayer uid rid playerMap'
