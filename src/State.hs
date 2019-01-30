module State where
-- This module will be pure functions related to manipulating the GlobalState

import Control.Concurrent
import Control.Concurrent.STM hiding (stateTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import qualified Data.Map.Strict as M
import Data.List (intersperse, find)
import Data.Text (Text)
import qualified Data.Text as T

import SqliteLib (formatUser) 
import Types ( ActiveUsers
             , Error(..)
             , GlobalState(..)
             , PlayerMap
             , ThreadEnv(..)
             , User(..)
             , UserId
             , RoomId
             )


maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing = Left a

----------------------------------
---- GlobalState Manipulation ----
----------------------------------

--- Impure Getter/Setters ---
readState :: ReaderT ThreadEnv IO GlobalState
readState = do
    stateTVar <- asks threadEnvStateTVar
    liftIO $ readTVarIO stateTVar

setState :: GlobalState -> ReaderT ThreadEnv IO ()
setState state = do
    stateTVar <- asks threadEnvStateTVar
    liftIO . atomically $ writeTVar stateTVar state

getUserId :: ReaderT ThreadEnv IO (Either Error UserId)
getUserId = do
    activeUsers <- globalActiveUsers <$> readState
    thread <- liftIO myThreadId
    return $ userUserId <$> getUserByThread thread activeUsers

---- Pure Getter/Setters ---
    
--- ActiveUsers ---

removeUser :: UserId -> ActiveUsers -> ActiveUsers
removeUser = M.delete 

addUser :: User -> ThreadId -> ActiveUsers -> ActiveUsers
addUser user tid activeUsers = 
    let uid = userUserId user
    in M.insert uid (user, tid) activeUsers

getUserByThread :: ThreadId -> ActiveUsers -> Either Error User
getUserByThread tid activeUsers =
    let users :: [(UserId, (User, ThreadId))]
        users = M.toList activeUsers
        f :: (UserId, (User, ThreadId)) -> Bool
        f (_, (_, tid')) = tid == tid'
    in case fst . snd <$> find f users of
        Nothing -> Left NotLoggedIn
        Just user -> Right user


whois :: GlobalState -> Text
whois state =
   let users = M.elems $ globalActiveUsers state
       formatedUsers = formatUser . fst <$> users
   in T.concat . intersperse (T.pack "\n") $ formatedUsers

--- PlayerMap ---

replacePlayerMap :: GlobalState -> PlayerMap -> GlobalState
replacePlayerMap (GlobalState activeUsers' world _) =
    GlobalState activeUsers' world 

removePlayer :: UserId -> RoomId -> PlayerMap -> PlayerMap
removePlayer uid = 
    M.adjust (filter (/= uid)) 

addPlayer :: UserId -> RoomId -> PlayerMap -> PlayerMap
addPlayer uid = 
    M.adjust ((:) uid)

swapPlayer :: UserId -> RoomId -> RoomId -> PlayerMap -> PlayerMap
swapPlayer uid rid rid' =
    addPlayer uid rid' . removePlayer uid rid

findPlayer :: UserId -> PlayerMap -> Either Error (RoomId, UserId)
findPlayer uid playerMap' = 
    let f :: (RoomId, [UserId]) -> [(RoomId, UserId)]
        f (i, xs) = (,) i <$> xs
        players :: [(RoomId, UserId)]
        players = concatMap f (M.toList playerMap')
        g :: (RoomId, UserId) -> Bool
        g (_, uid') = uid == uid'
        player :: Maybe (RoomId, UserId)
        player = find g players
    in maybeToEither UserNotInPlayerMap player

findAndSwapPlayer :: UserId -> RoomId -> PlayerMap -> PlayerMap
findAndSwapPlayer uid rid playerMap' =
    case findPlayer uid playerMap' of
        Right (rid', _) -> swapPlayer uid rid' rid playerMap'
        _ -> addPlayer uid rid playerMap'


