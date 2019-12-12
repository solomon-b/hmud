module HMud.Types where

import Control.Concurrent (ThreadId)
import Control.Lens

import Data.List (intersperse)
import qualified Data.Map.Strict as M
import qualified Data.Text as T (concat, pack, append)
import Data.Map.Strict (Map)
import Database.SQLite.Simple
import Data.Text (Text)

import HMud.Errors
import qualified HMud.Socket as Socket

----------------
--- Commands ---
----------------

data Command
    = GetUsers
    | GetUser Text
    | AddUser User
    | Echo Text
    | Shutdown
    | Register
    | Raw Text
    | Look Target
    | Login
    | Logout
    | Exit
    | Help
    | Whois
    | Say Text
    | Emote Text
    | Move Direction
    | Word Text
    | SuppressEcho
    | UnsuppressEcho
    deriving (Eq, Show)

data Target = Object Text | Dir Direction | Here
  deriving (Eq, Show)

data Direction =
    N | S | E | W | NW | NE | SW | SE | U | D deriving (Eq, Ord)

instance Show Direction where
    show U  = "Up"
    show D  = "Down"
    show N  = "North"
    show S  = "South"
    show E  = "East"
    show W  = "West"
    show NW = "Northwest"
    show NE = "Northeast"
    show SW = "Southwest"
    show SE = "Southeast"

-------------------
---- State/Env ----
-------------------

newtype ItemTypeId = ItemTypeId Integer deriving (Show, Eq, Ord)
newtype ItemId = ItemId Integer deriving (Show, Eq, Ord)
newtype InventoryId = InventoryId Integer deriving (Show, Eq, Ord)
newtype RoomId = RoomId Integer deriving (Show, Eq, Ord)
newtype PlayerId = PlayerId Integer deriving (Show, Eq, Ord)

type ActivePlayers = Map PlayerId Player
type UserMap       = Map UserId PlayerId
type WorldMap      = Map RoomId Room
type PlayerMap     = Map RoomId [PlayerId]
type InventoryMap  = Map InventoryId [ItemId]
type ItemTypeMap   = Map ItemTypeId ItemType
type ItemMap       = Map ItemId Item

type TwoHanded = Bool
data EquipmentType = Head | Torso | Legs | Feet | Hand TwoHanded | Arms | Finger
  deriving Show

data ItemType = ItemType
  { _itemTypeName              :: Text
  , _itemTypeItemTypeId        :: ItemTypeId
  , _itemTypeContainerCapacity :: Integer
  , _itemTypeWeight            :: Integer
  , _itemTypeIsEquippable      :: Bool
  , _itemTypeEquipmentType     :: EquipmentType
  , _itemTypeMovable           :: Bool
  , _itemTypeDescription       :: Text
  } deriving Show

data Item = Item
  { _itemItemId       :: ItemId
  , _itemItemTypeId   :: ItemTypeId
  , _itemInventoryId  :: Maybe InventoryId
  } deriving Show

type UserId = Integer

data User = User
  { _userUserId   :: Integer
  , _userUsername :: Text
  , _userPassword :: Text
  } deriving (Eq)

data Player = Player
  { _playerPlayerId    :: PlayerId
  , _playerName        :: Text
  , _playerUserId      :: UserId
  , _playerDescription :: Text
  , _playerInventoryId :: InventoryId
  } deriving (Eq, Show)


data GameState = GameState
  { _gsActivePlayers :: ActivePlayers
  , _gsUserMap       :: UserMap
  , _gsWorldMap      :: WorldMap
  , _gsPlayerMap     :: PlayerMap
  , _gsInventoryMap  :: InventoryMap
  , _gsItemTypeMap   :: ItemTypeMap
  , _gsItemMap       :: ItemMap
  } deriving Show

---------------------------
--- SQLite Transformers ---
---------------------------

instance FromRow User where
  fromRow = User <$> field <*> field <*> field
instance ToRow User where
  toRow (User _ username' password') = toRow (username', password')
instance Show User where
  show user = show (_userUsername user)
instance FromRow Player where
  fromRow = Player <$> (PlayerId <$> field) <*> field <*> field <*> field <*> (InventoryId <$> field)
instance ToRow Player where
  toRow (Player (PlayerId t1) t2 t3 t4 (InventoryId t5)) = toRow (t1, t2, t3, t4, t5)

-------------------
---- The World ----
-------------------

type Name = Text

type Description = Text

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
instance TShow Room where
  tshow = T.pack . show

newtype RoomText = RoomText { getRoomText :: Text } deriving Show


------------------------
---- Response Types ----
------------------------

data Response
  = RespSay Username Msg
  | RespHelp
  | RespLook Text
  | RespAnnounce Text
  | Prompt Text
  | RespRegister Username
  | RespPlayerCreated Text
  | RespShutdown
  | RespExit ThreadId Socket.Handle
  | RespLogout
  | RespAppError AppError

instance Show Response where
  show = \case
    RespSay user msg    -> concat ["<", show user, "> ", show msg]
    RespLook text       -> show text
    RespAnnounce text   -> show text
    Prompt text         -> show text
    RespRegister u      -> "Account Registration Successful: " ++ show u
    RespPlayerCreated p -> "Player created: " ++ show p
    RespShutdown        -> "RespShutdown"
    RespExit thread _   -> "Closing thread: " ++ show thread
    RespLogout          -> "Goodbye!"
    RespAppError err    -> show err
    RespHelp            -> foldMap show availableCommands

availableCommands :: [Text]
availableCommands = intersperse "\r\n" commands
  where
    commands :: [Text]
    commands =
      [ "Available Commands:"
      , "say <text> <-- Global Chat"
      , "look <-- Examine things"
      , "north,south,east,west,up,down <-- Movement"
      , "exit <-- Disconnect from server"
      , "logout <-- Logout to main menu"
      ]

instance TShow Response where
  tshow (RespSay uname msg) = T.concat ["<", uname, "> ", msg, "\r\n"]
  tshow (RespLook text)     = T.append text "\r\n"
  tshow (RespAnnounce text) = T.append text "\r\n"
  tshow (Prompt text)       = text
  tshow (RespHelp)          = T.append (T.concat availableCommands) "\r\n"
  tshow resp                = T.pack $ show resp ++ "\r\n"


type Msg = Text
type Username = Text

class TShow a where
  tshow :: a -> Text

----------------
--- Lookable ---
----------------

class Lookable a where
  look :: a -> Text

instance Lookable Room where
  look = tshow

instance Lookable Player where
  look = _playerName

instance Lookable Item where
  look = T.pack . show

--------------
--- Lenses ---
--------------

makeLenses ''GameState
makeLenses ''Room
makeLenses ''Item
makeLenses ''Player
makeLenses ''User
makeLenses ''ItemType
