module Types where
    
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Control.Concurrent (ThreadId)
import Control.Concurrent.STM (TVar, TChan)
import Control.Exception (Exception)
import Network.Socket (Socket)
import Database.SQLite.Simple (Connection, FromRow(..), ToRow(..), field)
import Database.SQLite.Simple.Types


-------------------
---- State/Env ----
-------------------

data Env = 
    Env { envConn      :: Connection
        , envSock      :: Socket
        , envStateTVar :: TVar GlobalState
        , envWChannel  :: TChan Msg
        } 

data ThreadEnv =
    ThreadEnv { threadEnvConn      :: Connection
              , threadEnvSock      :: Socket
              , threadEnvStateTVar :: TVar GlobalState
              , threadEnvWChannel  :: TChan Msg
              , threadEnvRChannel  :: TChan Msg
              }

type Msg = String
type Username = Text

newtype GlobalState = 
    GlobalState { globalActiveUsers :: Map Username (User, ThreadId) } 
    deriving Show


----------------
---- Parser ----
----------------

data Command = 
      GetUsers
    | GetUser Text
    | AddUser User
    | Echo Text
    | Exit
    | Shutdown
    | Logout
    | Whois
    | Say Text
    deriving (Eq, Show)


------------------
---- Database ----
------------------

data User =
    User { userUserId   :: Integer
         , userUsername :: Text
         , userPassword :: Text
         } deriving (Eq, Show)

type UserRow = (Null, Text, Text)

data DuplicateData = DuplicateData 
    deriving (Eq, Show, Typeable)

instance Exception DuplicateData

instance FromRow User where
    fromRow = User <$> field <*> field <*> field

instance ToRow User where
    toRow (User id_ username' password') = toRow (id_, username', password')

