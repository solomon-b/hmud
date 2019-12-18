# Sketch of an updated state
```
newtype ItemTypeId = ItemTypeId Integer
newtype ItemId = ItemId Integer
newtype InventoryId = InventoryId Integer
newtype RoomId = RoomId Integer
newtype PlayerId = PlayerId Integer

type TwoHanded = Bool

data EquipmentType = Head | Torso | Legs | Feet | Hand TwoHanded | Arms | Finger

data ItemType =
  { _name              :: Text
  , _itemTypeId        :: ItemTypeId
  , _containerCapacity :: Nat
  , _weight            :: Nat
  , _equipmentType     :: Maybe EquipmentType
  , _movable           :: Bool
  , _description       :: Text
  }

data Item = 
  { _itemId       :: ItemId
  , _itemTypeId   :: ItemTypeId
  , _inventoryId  :: Maybe InventoryId
  }

data Room =
  { _name        :: RoomId
  , _roomId      :: Integer
  , _description :: Text
  , _neighbors   :: [(Dir, RoomId)]
  , _inventoryId :: InventoryId
  }

data Player = Player
  { _name        :: Text
  , _playerId    :: playerId
  , _userId      :: UserId
  , _description :: Text
  , _inventoryId :: InventoryId
  }

data GlobalState =
  { _players       :: Map PlayerId Player
  , _worldMap      :: Map RoomId Room
  , _playerMap     :: Map RoomId [UserId]
  , _InventoryMap  :: Map InventoryId [ItemId]
  , _ItemTypeMap :: Map ItemTypeId ItemType
  , _itemMap     :: Map ItemId Item
  }
```


# Env
```
 - envConn      :: Connection
 - envSock      :: Socket
 - envStateTVar :: TVar GameState
 - envPubTChan  :: TChan Msg
```


# UserEnv
```
 - uEnvConnHandle     :: Sql.Handle          -- Database Connection
 - uEnvSockHandle     :: Socket.Handle       -- TCP Socket
 - uEnvStateTVar      :: TVar GameState      -- Shared State
 - uEnvPubTChan       :: TChan Msg           -- Public Message Channel
 - uEnvSockReadTChan  :: TChan Command       -- User Socket Read Channel
 - uEnvSockWriteTChan :: TChan Response      -- User Socket Write Channel
 - uEnvUserId         :: TVar (Maybe UserId) -- Current User ID, should this be Maybe User?
```


# GameState
```
- globalActiveUsers :: ActiveUsers -- Map UserId (User, ThreadId)
- globalWorld       :: World       -- Room Graph
- globalPlayerMap   :: PlayerMap   -- Map RoomId [UserId]
```


# MTL Typeclasses
```
- MonadIO => MonadTCP       -- Open/Close/Accept/SendTo Socket
- MonadIO => MonadDB        -- CRUD Database Interface
- MonadIO => MonadTChan     -- Read/Write/Dupe TChan
- MonadIO => MonadStateMvar -- Read/Write to State MVar
- Monad m => MonadGameState -- A beastiary of pure state manipulations; will need to be subclassed.
```


# IO Actions
```
1. Send To Socket      -- MonadIO => MonadTCP
2. Read From Socket    -- MonadIO => MonadTCP
3. Query Connection    -- MonadIO => MonadDB
4. Mutation Connection -- MonadIO => MonadDB
5. Read TVar           -- MonadIO => MonadStateTVar
6. Write TVar          -- MonadIO => MonadStateTVar
7. Read From TChan     -- MonadIO => MonadTChan
8. Write to TChan      -- MonadIO => MOnadTChan
```


# User Threads
```
1. Execution Loop    -- Reads commands from Internal TChan and executes non-TCP actions.
2. Socket Read Loop  -- Reads from the TCP Socket and writes to the Internal TChan.
3. Socket Write Loop -- Reads from the Public and User TChans and writes to the TCP Socket.
```


# Submodules
```
- Telnet Handler -- Pure: Preprocesses Telnet Bytestrings
- Parser         -- Pure: Parses Bytestrings into Commands
- CommandExec    -- MonadGameState: Manipulate GameState based on Commands
- World          -- MonadDB: Reads the world from the database (and performs world modifications/snapshots?)
- Prompts        -- MonadTChan: Reads incoming messages from the User Read Socket
- Dispatcher     -- MonadTCP: Processes and sends all messages/text to the Socket
```


# Player Actions
```
- Movement => Modify GameState, send Text to User Write Channel
- Looking  => Read GameState/World, sent Text to User Write Channel
- Speaking => Send Text to Public Message Channel
```
Many more actions to follow once the initial framework is established.


# Messages
```
Messages are tagged by with intended audience and have their own Show instances:
- Admin  -- All users
- Global -- All users
- Direct -- A specific user
- Local  -- All users in the current room
```
