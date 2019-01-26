module State where

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
             )

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


--- Pure Getter/Setters ---

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


