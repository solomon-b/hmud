module Types where
    
import qualified Data.Map.Strict as M
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
              , threadEnvUserId    :: TVar (Maybe UserId)
              }

type Msg = Text
type Username = Text
type ActiveUsers = Map UserId (User, ThreadId)

data GlobalState = 
    GlobalState { globalActiveUsers :: ActiveUsers
                , globalWorld       :: World
                , globalPlayerMap   :: PlayerMap
                } deriving Show


----------------
---- Errors ----
----------------

data Error = NoSuchUser
           | NoSuchRoom
           | NotLoggedIn
           | AlreadyLoggedIn
           | UserNotFound
           | InvalidPassword
           | PasswordsDontMatch
           | UserNotInPlayerMap
           | InvalidCommand
           | BadParse String
           deriving Show


-------------------
---- The World ----
-------------------

data Direction = 
    N | S | E | W | NW | NE | SW | SE | U | D deriving (Eq, Ord)

instance Show Direction where
    show U = "Up"
    show D = "Down"
    show N = "North"
    show S = "South"
    show E = "East"
    show W = "West"
    show NW = "Northwest"
    show NE = "Northeast"
    show SW = "Southwest"
    show SE = "Southeast"

type Name = Text
type Description = Text
type RoomId = Integer
type World = Map RoomId Room
type PlayerMap = Map RoomId [UserId]

data Room = 
    Room { roomName        :: Name
         , roomDescription :: Description
         , roomRoomId      :: RoomId
         , roomAdjacent    :: Map Direction RoomId
         }

instance Show Room where
    show (Room name desc _ dir) =
        show name ++ "\n" ++
        show desc ++ "\n" ++
        "Exits: " ++ show (M.keys dir)

newtype RoomText = RoomText { getRoomText :: Text } deriving Show


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
    | Register
    | Look
    | Login
    | Logout
    | Whois
    | Say Text
    | Move Direction
    deriving (Eq, Show)


------------------
---- Database ----
------------------
type UserId = Integer

data User =
    User { userUserId   :: Integer
         , userUsername :: Text
         , userPassword :: Text
         } deriving Eq

type UserRow = (Null, Text, Text)

data DuplicateData = DuplicateData 
    deriving (Eq, Show, Typeable)

instance Exception DuplicateData

instance FromRow User where
    fromRow = User <$> field <*> field <*> field

instance ToRow User where
    toRow (User id_ username' password') = toRow (id_, username', password')

instance Show User where
    show user = show (userUsername user)
