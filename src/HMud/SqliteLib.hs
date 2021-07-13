{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module HMud.SqliteLib
    ( Handle(..)
    , Config(..)
    , Account(..)
    , AccountId
    , withHandle
    , createDatabase
    , insertAccount
    , selectAccount
    , selectAllAccounts
    , formatAccount
    , insertPlayer
    , selectPlayer
    , selectPlayerByAccountId
    , selectAllPlayers
    , MonadSqlCRUD(..)
    , HasConnectionHandle(..)
    , runTests
    ) where

import Control.Exception (Exception, bracket, throwIO)
import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader

import Data.Text (Text, concat, pack)
import Data.Typeable (Typeable)
import Database.SQLite.Simple
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.ToField
import Text.RawString.QQ

import HMud.Types

{-
tachema:

CREATE TABLE IF NOT EXISTS accounts(
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  email     TEXT UNIQUE NOT NULL,
  password  TEXT NOT NULL,
  FOREIGN KEY(player) REFERENCES(players.id)
);

CREATE TABLE IF NOT EXISTS players (
  id                   INTEGER PRIMARY KEY AUTOINCREMENT,
  FOREIGN KEY(account) REFERENCES(accounts.id),
  name                 TEXT NOT NULL,
  description          TEXT NOT NULL
);

CREATE TABLE inventory(
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  ownerId     INTEGER NOT NULL,
  ownerType   TEXT NOT NULL,
  capacity    INTEGER NOT NULL
);

CREATE TABLE items(
  id                     INTEGER PRIMARY KEY AUTOINCREMENT,
  FOREIGN KEY(itemType)  REFERENCES(itemTypes.id) NOT NULL,
  FOREIGN KEY(inventory) REFERENCES(inventory.id) NOT NULL,
);

CREATE TABLE itemTypes(
  id                INTEGER PRIMARY KEY AUTOINCREMENT,
  name              TEXT NOT NULL,
  description       TEXT NOT NULL,
  weight            INTEGER NOT NULL,
  equipmentType     TEXT NOT NULL,
  movable           BOOLEAN NOT NULL,
  containerCapacity INTEGER NOT NULL
);

CREATE TABLE rooms(
  id          INTEGER PRIMARY KEY AUTOINCREMENT,
  name        TEXT NOT NULL,
  description TEXT NOT NULL
);

CREATE TABLE RoomAdjacency(
  PRIMARY KEY (startingRoom, endingRoom),
  FOREIGN KEY(startingRoom) REFERENCES(rooms.id) NOT NULL,
  FOREIGN KEY(endingRoom)   REFERENCES(rooms.id) NOT NULL,
  direction                 TEXT NOT NULL
);

-}


----------------
---- Handle ----
----------------

newtype Handle = Handle { hConnection :: Connection }

newtype Config = Config { cDatabase :: String }

newHandle :: Config -> IO Handle
newHandle (Config db) = Handle <$> open db

closeHandle :: Handle -> IO ()
closeHandle (Handle db) = close db

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config = bracket (newHandle config) closeHandle

---------------
---- Types ----
---------------

--type AccountRow = (Null, Text, Text)

data DuplicateData = DuplicateData
    deriving (Eq, Show, Typeable)

instance Exception DuplicateData

-----------------
---- Queries ----
-----------------

-- Create Tables --

createAccountTableQuery :: Query
createAccountTableQuery = [r|
CREATE TABLE IF NOT EXISTS accounts(
  id                    INTEGER PRIMARY KEY AUTOINCREMENT,
  email                 TEXT UNIQUE NOT NULL,
  password              TEXT NOT NULL,
  playerId              INTEGER,
  FOREIGN KEY(playerId) REFERENCES players (id)
)
|]

createPlayerTableQuery :: Query
createPlayerTableQuery = [r|
CREATE TABLE IF NOT EXISTS players(
  id                     INTEGER PRIMARY KEY AUTOINCREMENT,
  accountId              INTEGER NOT NULL,
  name                   TEXT NOT NULL,
  description            TEXT NOT NULL,
  FOREIGN KEY(accountId) REFERENCES accounts (id)
)
|]

createInventoryTableQuery :: Query
createInventoryTableQuery = [r|
CREATE TABLE inventories(
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  ownerId     INTEGER NOT NULL,
  ownerType   TEXT NOT NULL,
  capacity    INTEGER NOT NULL)
|]

createItemTableQuery :: Query
createItemTableQuery = [r|
CREATE TABLE items(
  id                     INTEGER PRIMARY KEY AUTOINCREMENT,
  FOREIGN KEY(itemType)  REFERENCES(itemTypes.id) NOT NULL,
  FOREIGN KEY(inventory) REFERENCES(inventory.id) NOT NULL,
);
|]

-- Account Queries --

insertAccountQuery :: Query
insertAccountQuery = [r|
INSERT INTO accounts
 VALUES (NULL, :email, :pass, NULL)
|]

deleteAccountQuery :: Query
deleteAccountQuery = [r|
DELETE FROM accounts
WHERE id = :id
|]

setAccountQuery :: Query
setAccountQuery = [r|
UPDATE accounts
SET email = :email, password = :pass, playerId = :playerId
WHERE id = :id
|]

selectAllAccountsQuery :: Query
selectAllAccountsQuery = "SELECT * from accounts"

selectAccountByEmailQuery :: Query
selectAccountByEmailQuery = "SELECT * from accounts where email = ?"

selectAccountByIdQuery :: Query
selectAccountByIdQuery = "SELECT * from accounts where id = ?"

-- Player Queries --

insertPlayerQuery :: Query
insertPlayerQuery = [r|
INSERT INTO players
VALUES (NULL, :accountId, :name, :desc)
|]

deletePlayerQuery :: Query
deletePlayerQuery = [r|
DELETE FROM players
WHERE id = :id
|]

setPlayerQuery :: Query
setPlayerQuery = [r|
UPDATE accounts
SET accountId = :accountId, name = :name, description = :desc
WHERE id = :id
|]

selectAllPlayersQuery :: Query
selectAllPlayersQuery = [r|
SELECT
  players.id,
  players.accountId,
  players.name,
  players.description,
  inventories.id
FROM
  players
LEFT JOIN
  inventories
ON
  inventories.ownerId = players.id
|]

selectPlayerQueryByName :: Query
selectPlayerQueryByName = [r|
SELECT *
FROM players
INNER JOIN inventories ON inventories.ownerId = players.id
WHERE players.name = ?
|]

selectPlayerByAccountIdQuery :: Query
selectPlayerByAccountIdQuery = [r|
SELECT *
FROM players
INNER JOIN inventories ON inventories.ownerId = players.id
WHERE accountId = ?
|]

selectPlayerByIdQuery :: Query
selectPlayerByIdQuery = [r|
SELECT *
FROM players
INNER JOIN inventories ON inventories.ownerId = players.id
WHERE id = ?
|]

-- Inventory Queries --

insertInventoryQuery :: Query
insertInventoryQuery = [r|
INSERT INTO inventories
  VALUES (NULL, :ownerId, :ownerType, :capacity)
|]

deleteInventoryQuery :: Query
deleteInventoryQuery = [r|
DELETE FROM inventories
WHERE id = :id
|]

setInventoryQuery :: Query
setInventoryQuery = [r|
UPDATE inventories
SET ownerId = :ownerId, ownerType = :ownerType, capacity = :capacity
WHERE id = :id
|]

selectAllInventoriesQuery :: Query
selectAllInventoriesQuery = [r|
SELECT * FROM inventories
|]

selectInventoryByIdQuery :: Query
selectInventoryByIdQuery = [r|
SELECT * FROM inventories
WHERE id = :id
|]

selectInventoryByOwnerQuery :: Query
selectInventoryByOwnerQuery = [r|
SELECT * FROM inventories
WHERE ownerId = :ownerId AND ownerType = :ownerType
|]

-- Item Queries --
insertItemQuery :: Query
insertItemQuery = [r|
INSERT INTO items
  VALUES (NULL, :itemTypeId, :inventoryId)
|]

deleteItemQuery :: Query
deleteItemQuery = [r|
DELETE FROM items
WHERE id = :id
|]

setItemQuery :: Query
setItemQuery = [r|
UPDATE items
SET ownerId = :ownerId, ownerType = :ownerType, capacity = :capacity
WHERE id = :id
|]

-----------------------
---- Database CRUD ----
-----------------------

createDatabase :: IO ()
createDatabase = do
    conn <- open "hmud.db"
    execute_ conn createAccountTableQuery
    execute_ conn createPlayerTableQuery
    execute_ conn createInventoryTableQuery
    --execute conn insertAccountQuery defAccount
    users <- query_ conn selectAllAccountsQuery
    mapM_ print (users :: [Account])
    SQLite.close conn
    --where defAccount :: AccountRow
    --      defAccount = (Null, "solomon", "pass")

-- TODO: These pure functions should probably go somewhere else?
formatAccount :: Account -> Text
formatAccount (Account uid email _ playerId) =
    Data.Text.concat [ "Email: ", email, "\t"
                     , "UID: "  , (pack . show) uid, "\t"
                     , "playerId: ", (pack . show) playerId]


-------------------
--- Test Script ---
-------------------

--viewDB :: (MonadSqlCRUD m r i, MonadReader env m, MonadIO m) => Handle -> m ()
viewDB :: Handle -> ReaderT env IO ()
viewDB conn = do
  accounts <- viewIf conn (const True) :: ReaderT env IO [Account]
  players <- viewIf conn (const True) :: ReaderT env IO [Player]
  inventories <- viewIf conn (const True) :: ReaderT env IO [Inventory]
  liftIO $ putStrLn "Accounts:"
  liftIO $ print accounts
  liftIO $ putStrLn "Players:"
  liftIO $ print players
  liftIO $ putStrLn "Inventories:"
  liftIO $ print inventories

createAnAccount :: Handle -> ReaderT env IO Account
createAnAccount conn = do
  liftIO $ putStrLn "Creating an account"
  create conn (Account (AccountId 1) "test@gmail.com" "password" Nothing)

createAPlayer :: Handle -> AccountId -> ReaderT env IO Player
createAPlayer conn aid = do
  liftIO $ putStrLn "Creating a player"
  create conn (Player (PlayerId 1) aid "Test" "Desc" (InventoryId 1))

deleteAnAccount :: Handle -> AccountId -> ReaderT env IO ()
deleteAnAccount conn aid = do
  liftIO $ putStrLn "Deleting an account"
  delete conn aid

deleteAPlayer :: Handle -> PlayerId -> ReaderT env IO ()
deleteAPlayer conn pid = do
  liftIO $ putStrLn "Deleting a player"
  delete conn pid

runTests :: IO ()
runTests = do
  conn <- open ""
  execute_ conn createAccountTableQuery
  execute_ conn createPlayerTableQuery
  execute_ conn createInventoryTableQuery
  let handle = Handle conn
  account <- runReaderT (createAnAccount handle) []
  player <- runReaderT (createAPlayer handle (account ^. accountId)) []
  runReaderT (viewDB handle) []
  --putStrLn ""
  --deleteAnAccount handle (account ^. accountId)
  --deleteAPlayer handle (player ^. playerPlayerId)
  --viewDB handle

-----------------
---- Actions ----
-----------------

class HasConnectionHandle env where
  getConnectionHandle :: env -> Handle

instance HasConnectionHandle Handle where
  getConnectionHandle = id

class Monad m => MonadSqlCRUD m rowType id | rowType -> id, id -> rowType where
  create :: Handle -> rowType -> m rowType
  delete :: Handle -> id -> m ()
  view   :: Handle -> id -> m (Maybe rowType)
  viewIf :: Handle -> (rowType -> Bool) -> m [rowType]
  over   :: Handle -> (rowType -> rowType) -> id -> m (Maybe rowType)
  over handle f uid =
      (fmap . fmap) f (view handle uid) >>= \case
        Just row -> set handle row
        Nothing -> pure Nothing
  set    :: Handle -> rowType  -> m (Maybe rowType)


instance MonadIO m => MonadSqlCRUD (ReaderT env m) Account AccountId where
  create (Handle conn) account = do
    let params = [ ":email" := account ^. accountEmail
                 , ":pass"  := account ^. accountPassword
                 ]
    liftIO $ executeNamed conn insertAccountQuery params
    pure account
  delete (Handle conn) (AccountId aid) = liftIO $ executeNamed conn deleteAccountQuery [":id" := aid]
  view handle uid = liftIO $ viewRow_ selectAccountByIdQuery handle uid
  viewIf (Handle conn) f = do
    accounts <- liftIO $ query_ conn selectAllAccountsQuery
    pure $ filter f accounts
  set (Handle conn) acc = do
    let params = [ ":id" := _accountId acc
                 , ":email" := _accountEmail acc
                 , ":pass" := _accountPassword acc
                 , ":playerId" := _accountPlayerId acc
                 ]
    liftIO $ executeNamed conn setAccountQuery params
    pure (Just acc)

instance MonadIO m => MonadSqlCRUD (ReaderT env m) Player PlayerId where
  create (Handle conn) player = do
    let params = [ ":accountId" := (player ^. playerAccountId)
                 , ":name"      := (player ^. playerName)
                 , ":desc"      := (player ^. playerDescription)
                 ]
    liftIO $ executeNamed conn insertPlayerQuery params
    pid <- liftIO $ (PlayerId . fromIntegral) <$> lastInsertRowId conn
    void $ create (Handle conn) (Inventory (InventoryId 0) 100 (PlayerOwned pid) [])
    iid <- liftIO $ (InventoryId . fromIntegral) <$> lastInsertRowId conn
    liftIO $ pure $ player & playerInventory .~ iid
  delete (Handle conn) (PlayerId pid) = liftIO $ executeNamed conn deletePlayerQuery [":id" := pid]
  view handle pid = liftIO $ viewRow_ selectPlayerByIdQuery handle pid
  viewIf (Handle conn) f = do
    players <- liftIO $ query_ conn selectAllPlayersQuery
    pure $ filter f players
  set (Handle conn) player = do
    let params = [ ":id" := _playerPlayerId player
                 , ":accountId" := _playerAccountId player
                 , ":name" := _playerName player
                 , ":desc" := _playerDescription player
                 --, ":invId" := _playerInventory player
                 ]
    void . liftIO $ executeNamed conn setPlayerQuery params
    pure (Just player)

instance MonadIO m => MonadSqlCRUD (ReaderT env m) Inventory InventoryId where
  create (Handle conn) inv = do
    let ownerType = case _inventoryOwnerId inv of
          PlayerOwned _ -> PlayerOwner
          RoomOwned   _ -> RoomOwner
          ItemOwned   _ -> ItemOwner
        params = [ ":ownerId" := _inventoryOwnerId inv
                 , ":ownerType" := ownerType
                 , ":capacity" := _inventoryCapacity inv
                 ]
    void . liftIO $ executeNamed conn insertInventoryQuery params
    pure inv
  delete (Handle conn) iid = liftIO $ execute conn deletePlayerQuery (Only iid)
  view handle iid = liftIO $ viewRow_ selectInventoryByIdQuery handle iid
  viewIf (Handle conn) f = do
    inventories <- liftIO $ query_ conn selectAllInventoriesQuery
    pure $ filter f inventories
  set (Handle conn) inv = do
    let ownerType = case _inventoryOwnerId inv of
          PlayerOwned _ -> PlayerOwner
          RoomOwned   _ -> RoomOwner
          ItemOwned   _ -> ItemOwner
        params = [ ":ownerId" := _inventoryOwnerId inv
                 , ":ownerType" := ownerType
                 , ":capacity" := _inventoryCapacity inv
                 ]
    void . liftIO $ executeNamed conn setInventoryQuery params
    pure (Just inv)

viewRow_ :: (ToField a, FromRow b) => Query -> Handle -> a -> IO (Maybe b)
viewRow_ query' (Handle conn) uid = query conn query' (Only uid) >>= \case
    [] -> return Nothing
    [row] -> return $ Just row
    _ -> throwIO DuplicateData

selectAccount :: (MonadIO m, MonadSqlCRUD m Account AccountId) => Handle -> Text -> m (Maybe Account)
selectAccount handle email = do
  xs <- (viewIf handle ((== email) . _accountEmail))
  case xs of
    [] -> pure Nothing
    x:[] -> pure $ Just x
    _ -> liftIO $ throwIO DuplicateData

selectAllAccounts :: MonadSqlCRUD m Account AccountId => Handle -> m [Account]
selectAllAccounts = flip viewIf (const True)

insertAccount :: MonadSqlCRUD m Account AccountId => Handle -> Account -> m Account
insertAccount = create

selectPlayer ::
  ( MonadIO m
  , MonadSqlCRUD m Player PlayerId
  ) => Handle -> Text -> m (Maybe Player)
selectPlayer handle name = do
  results <- (viewIf handle ((== name) . _playerName))
  case results of
    [] -> pure Nothing
    [player] -> pure $ Just player
    _ -> liftIO $ throwIO DuplicateData

selectPlayerByAccountId ::
  ( MonadIO m
  , MonadSqlCRUD m Player PlayerId
  ) => Handle -> AccountId -> m (Maybe Player)
selectPlayerByAccountId handle aid = do
    results <- viewIf handle ((== aid) . _playerAccountId)
    case results of
        [] -> return Nothing
        [player] -> return $ Just player
        _ -> liftIO $ throwIO DuplicateData

selectPlayerById :: MonadSqlCRUD m Player PlayerId => Handle -> PlayerId -> m (Maybe Player)
selectPlayerById handle = view handle

selectAllPlayers :: MonadSqlCRUD m Player PlayerId => Handle -> m [Player]
selectAllPlayers = flip viewIf (const True)

insertPlayer :: MonadSqlCRUD m Player PlayerId => Handle -> Player -> m Player
insertPlayer = create
