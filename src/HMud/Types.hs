module HMud.Types where

import Control.Concurrent (ThreadId)
import Control.Lens

import Data.List (intersperse)
import qualified Data.Map.Strict as M
import qualified Data.Text as T (concat, pack, append)
import Data.Map.Strict (Map)
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Data.Text (Text)

import HMud.Errors
import qualified HMud.Socket as Socket

----------------
--- Commands ---
----------------

data Command
    = GetAccounts
    | GetAccount Text
    | AddAccount Account
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
newtype AccountId = AccountId Integer deriving (Show, Eq, Ord)
newtype PlayerId = PlayerId Integer deriving (Show, Eq, Ord)

type ActivePlayers = Map PlayerId Player
type WorldMap      = Map RoomId Room
type PlayerMap     = Map RoomId [PlayerId]
type InventoryMap  = Map InventoryId [ItemId]
type ItemTypeMap   = Map ItemTypeId ItemType
type ItemMap       = Map ItemId Item

type TwoHanded = Bool
data EquipmentType = Head | Torso | Legs | Feet | Hand TwoHanded | Arms | Finger | Trinket
  deriving Show

data ItemType = ItemType
  { _itemTypeItemTypeId    :: ItemTypeId
  , _itemTypeName          :: Text
  , _itemTypeDescription   :: Text
  , _itemTypeWeight        :: Integer
  , _itemTypeEquipmentType :: EquipmentType
  , _itemTypeMovable       :: Bool
  , _itemContainerCapacity :: Integer
  } deriving Show

data Item = Item
  { _itemItemId      :: ItemId
  , _itemItemTypeId  :: ItemTypeId
  , _itemInventoryId :: InventoryId
  } deriving Show

data Account = Account
  { _accountId   :: AccountId
  , _accountEmail    :: Text
  , _accountPassword :: Text
  , _accountPlayerId :: Maybe (PlayerId)
  } deriving Eq

data Player = Player
  { _playerPlayerId    :: PlayerId
  , _playerAccountId   :: AccountId
  , _playerName        :: Text
  , _playerDescription :: Text
  , _playerInventoryId :: Maybe InventoryId
  } deriving (Eq, Show)

instance Show Account where
  show = do
    aId <- show . _accountId
    aEmail <- show . _accountEmail
    aPid <- show . _accountPlayerId
    pure $ aId <> " " <> aEmail <> " " <> aPid

data GameState = GameState
  { _gsActivePlayers :: ActivePlayers
  , _gsWorldMap      :: WorldMap
  , _gsPlayerMap     :: PlayerMap
  , _gsInventoryMap  :: InventoryMap
  , _gsItemTypeMap   :: ItemTypeMap
  , _gsItemMap       :: ItemMap
  } deriving Show

---------------------------
--- SQLite Transformers ---
---------------------------

instance FromRow Account where
  fromRow = Account <$> (AccountId <$> field) <*> field <*> field <*> ((Just . PlayerId) <$> field)
instance ToRow Account where
  toRow (Account _ email password _) = toRow (email, password)

instance FromRow Player where
  fromRow = Player <$> (PlayerId <$> field) <*> (AccountId <$> field) <*> field <*> field <*> ((Just . InventoryId) <$> field)
instance ToRow Player where
  toRow (Player (PlayerId t1) (AccountId t2) t3 t4 (Just (InventoryId t5))) = toRow (t1, t2, t3, t4, t5)
  toRow (Player (PlayerId t1) (AccountId t2) t3 t4 Nothing) = toRow (t1, t2, t3, t4, SQLNull)

--instance FromRow (Map Direction RoomId) where
--  fromRow = undefined

--instance FromRow Room where
--  fromRow = Room <$> field <*> field <*> (RoomId <$> field) <*> field

instance ToField (Map Direction RoomId) where
  toField = SQLText . T.pack . show

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
  = RespSay Accountname Msg
  | RespHelp
  | RespLook Text
  | RespAnnounce Text
  | Prompt Text
  | RespRegister Accountname
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
type Accountname = Text

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
makeLenses ''Account
makeLenses ''ItemType
