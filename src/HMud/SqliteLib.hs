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
    , runTests
    ) where

import Control.Lens ((&), (.~), (^.))
import Control.Exception (Exception, bracket, throwIO)
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

viewDB :: Handle -> IO ()
viewDB conn = do
  accounts <- viewIf conn (const True) :: IO [Account]
  players <- viewIf conn (const True) :: IO [Player]
  inventories <- viewIf conn (const True) :: IO [Inventory]
  putStrLn "Accounts:"
  print accounts
  putStrLn "Players:"
  print players
  putStrLn "Inventories:"
  print inventories

createAnAccount :: Handle -> IO Account
createAnAccount conn = do
  putStrLn "Creating an account"
  create conn (Account (AccountId 1) "test@gmail.com" "password" Nothing)

createAPlayer :: Handle -> AccountId -> IO Player
createAPlayer conn aid = do
  putStrLn "Creating a player"
  create conn (Player (PlayerId 1) aid "Test" "Desc" (InventoryId 1))

deleteAnAccount :: Handle -> AccountId -> IO ()
deleteAnAccount conn aid = do
  putStrLn "Deleting an account"
  delete conn aid

deleteAPlayer :: Handle -> PlayerId -> IO ()
deleteAPlayer conn pid = do
  putStrLn "Deleting a player"
  delete conn pid

runTests :: IO ()
runTests = do
  conn <- open ""
  execute_ conn createAccountTableQuery
  execute_ conn createPlayerTableQuery
  execute_ conn createInventoryTableQuery
  let handle = Handle conn
  account <- createAnAccount handle
  player <- createAPlayer handle (account ^. accountId)
  viewDB handle
  --putStrLn ""
  --deleteAnAccount handle (account ^. accountId)
  --deleteAPlayer handle (player ^. playerPlayerId)
  --viewDB handle

-----------------
---- Actions ----
-----------------

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


instance MonadSqlCRUD IO Account AccountId where
  create (Handle conn) account = do
    let params = [ ":email" := account ^. accountEmail
                 , ":pass" := account ^. accountPassword]
    executeNamed conn insertAccountQuery params
    pure account
  delete (Handle conn) (AccountId aid) = executeNamed conn deleteAccountQuery [":id" := aid]
  view handle uid = viewRow_ selectAccountByIdQuery handle uid
  viewIf (Handle conn) f = do
    accounts <- query_ conn selectAllAccountsQuery
    pure $ filter f accounts
  set (Handle conn) acc = do
    let params = [ ":id" := _accountId acc
                 , ":email" := _accountEmail acc
                 , ":pass" := _accountPassword acc
                 , ":playerId" := _accountPlayerId acc
                 ]
    executeNamed conn setAccountQuery params
    pure (Just acc)

instance MonadSqlCRUD IO Player PlayerId where
  create (Handle conn) player = do
    let params = [ ":accountId" := (player ^. playerAccountId)
                 , ":name"      := (player ^. playerName)
                 , ":desc"      := (player ^. playerDescription)
                 ]
    executeNamed conn insertPlayerQuery params
    pid <- (PlayerId . fromIntegral) <$> lastInsertRowId conn
    create (Handle conn) (Inventory (InventoryId 0) 100 (PlayerOwned pid) [])
    iid <- (InventoryId . fromIntegral) <$> lastInsertRowId conn
    pure $ player & playerInventory .~ iid
    pure player
  delete (Handle conn) (PlayerId pid) = executeNamed conn deletePlayerQuery [":id" := pid]
  view handle pid = viewRow_ selectPlayerByIdQuery handle pid
  viewIf (Handle conn) f = do
    players <- query_ conn selectAllPlayersQuery
    pure $ filter f players
  set (Handle conn) player = do
    let params = [ ":id" := _playerPlayerId player
                 , ":accountId" := _playerAccountId player
                 , ":name" := _playerName player
                 , ":desc" := _playerDescription player
                 --, ":invId" := _playerInventory player
                 ]
    executeNamed conn setPlayerQuery params
    pure (Just player)

instance MonadSqlCRUD IO Inventory InventoryId where
  create (Handle conn) inv = do
    let ownerType = case _inventoryOwnerId inv of
          PlayerOwned _ -> PlayerOwner
          RoomOwned   _ -> RoomOwner
          ItemOwned   _ -> ItemOwner
        params = [ ":ownerId" := _inventoryOwnerId inv
                 , ":ownerType" := ownerType
                 , ":capacity" := _inventoryCapacity inv
                 ]
    executeNamed conn insertInventoryQuery params
    pure inv
  delete (Handle conn) iid = execute conn deletePlayerQuery (Only iid)
  view handle iid = viewRow_ selectInventoryByIdQuery handle iid
  viewIf (Handle conn) f = do
    inventories <- query_ conn selectAllInventoriesQuery
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
    executeNamed conn setInventoryQuery params
    pure (Just inv)

viewRow_ :: (ToField a, FromRow b) => Query -> Handle -> a -> IO (Maybe b)
viewRow_ query' (Handle conn) uid = query conn query' (Only uid) >>= \case
    [] -> return Nothing
    [row] -> return $ Just row
    _ -> throwIO DuplicateData

selectAccount :: Handle -> Text -> IO (Maybe Account)
selectAccount (Handle conn) name = do
    results <- query conn selectAccountByEmailQuery (Only name)
    case results of
        [] -> return Nothing
        [account] -> return $ Just account
        _ -> throwIO DuplicateData

selectAllAccounts :: Handle -> IO [Account]
selectAllAccounts (Handle conn) = query_ conn selectAllAccountsQuery

insertAccount :: Handle -> Account -> IO Account
insertAccount (Handle conn) account = do
    execute conn insertAccountQuery account
    return account

selectPlayer :: Handle -> Text -> IO (Maybe Player)
selectPlayer (Handle conn) name = do
    results <- query conn selectPlayerQueryByName (Only name)
    case results of
        [] -> return Nothing
        [player] -> return $ Just player
        _ -> throwIO DuplicateData

selectPlayerByAccountId :: Handle -> AccountId -> IO (Maybe Player)
selectPlayerByAccountId (Handle conn) (AccountId i) = do
    results <- query conn selectPlayerByAccountIdQuery (Only i)
    case results of
        [] -> return Nothing
        [player] -> return $ Just player
        _ -> throwIO DuplicateData

selectPlayerById :: Handle -> PlayerId -> IO (Maybe Player)
selectPlayerById (Handle conn) (PlayerId i) = do
    results <- query conn selectPlayerByIdQuery (Only i)
    case results of
        [] -> return Nothing
        [player] -> return $ Just player
        _ -> throwIO DuplicateData

selectAllPlayers :: Handle -> IO [Player]
selectAllPlayers (Handle conn) = query_ conn selectAllPlayersQuery

insertPlayer :: Handle -> Player -> IO Player
insertPlayer (Handle conn) player = do
    execute conn insertPlayerQuery player
    return player
