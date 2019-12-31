{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module HMud.SqliteLib
    ( Handle(..)
    , Config(..)
    , Account(..)
    , AccountId
    , withHandle
    , newHandle
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
    ) where

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

createAccountTableQuery :: Query
createAccountTableQuery = [r|
CREATE TABLE IF NOT EXISTS accounts(
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  email     TEXT UNIQUE NOT NULL,
  password  TEXT NOT NULL,
  playerId  INTEGER,
  FOREIGN KEY(playerId) REFERENCES players (id)
)
|]

insertAccountQuery :: Query
insertAccountQuery = [r|
INSERT INTO accounts
 VALUES (NULL, ?, ?, NULL)
|]

setAccountQuery :: Query
setAccountQuery = [r|
UPDATE accounts
SET email = :email, password = :pass, playerId = :playerId
WHERE id = :id
|]

deleteAccountQuery :: Query
deleteAccountQuery = [r|
DELETE FROM accounts
WHERE id = :id
|]

selectAllAccountsQuery :: Query
selectAllAccountsQuery = "SELECT * from accounts"

selectAccountByEmailQuery :: Query
selectAccountByEmailQuery = "SELECT * from accounts where email = ?"

selectAccountByIdQuery :: Query
selectAccountByIdQuery = "SELECT * from accounts where id = ?"

createPlayerTableQuery :: Query
createPlayerTableQuery = [r|
CREATE TABLE IF NOT EXISTS players (
  id                     INTEGER PRIMARY KEY AUTOINCREMENT,
  accountId              INTEGER,
  name                   TEXT NOT NULL,
  description            TEXT NOT NULL,
  FOREIGN KEY(accountId) REFERENCES accounts (id)
)
|]

insertPlayerQuery :: Query
insertPlayerQuery = [r|
INSERT INTO players
 VALUES (NULL, ?, ?, ?)
|]

setPlayerQuery :: Query
setPlayerQuery = [r|
UPDATE accounts
SET accountId = :accountId, name = :name, description = :desc
WHERE id = :id
|]

deletePlayerQuery :: Query
deletePlayerQuery = [r|
DELETE FROM players
WHERE id = :id
|]

selectAllPlayersQuery :: Query
selectAllPlayersQuery = "SELECT * from players"

selectPlayerQuery :: Query
selectPlayerQuery = "SELECT * from players where name = ?"

selectPlayerByAccountIdQuery :: Query
selectPlayerByAccountIdQuery = "SELECT * from players where accountId = ?"

selectPlayerByIdQuery :: Query
selectPlayerByIdQuery = "SELECT * from players where id = ?"

createInventoryTableQuery :: Query
createInventoryTableQuery = [r|
CREATE TABLE inventories(
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  ownerId     INTEGER NOT NULL,
  ownerType   TEXT NOT NULL,
  capacity    INTEGER NOT NULL)
|]

createInventoryQuery :: Query
createInventoryQuery = [r|
INSERT INTO inventories
  VALUES (NULL, ?, ?, ?)
|]

-----------------------
---- Database CRUD ----
-----------------------

createDatabase :: IO ()
createDatabase = do
    conn <-  open "hmud.db"
    execute_ conn createAccountTableQuery
    execute_ conn createPlayerTableQuery
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

-----------------
---- Actions ----
-----------------

class Monad m => MonadSqlCRUD m rowType id | rowType -> id, id -> rowType where
  create :: Handle -> rowType -> m rowType
  delete :: Handle -> id -> m ()
  view   :: Handle -> id -> m (Maybe rowType)
  over   :: Handle -> (rowType -> rowType) -> id -> m (Maybe rowType)
  over handle f uid =
      (fmap . fmap) f (view handle uid) >>= \case
        Just row -> set handle row
        Nothing -> pure Nothing
  set    :: Handle -> rowType  -> m (Maybe rowType)


instance MonadSqlCRUD IO Account AccountId where
  create (Handle conn) account = execute conn insertAccountQuery account >> pure account
  delete (Handle conn) aid = execute conn deleteAccountQuery (Only aid)
  view handle uid = viewRow_ selectAccountByIdQuery handle uid
  set (Handle conn) acc = do
    let params = [ ":id" := _accountId acc
                 , ":email" := _accountEmail acc
                 , ":pass" := _accountPassword acc
                 , ":playerId" := _accountPlayerId acc
                 ]
    executeNamed conn setAccountQuery params
    pure (Just acc)

instance MonadSqlCRUD IO Player PlayerId where
  create (Handle conn) player = execute conn insertPlayerQuery player >> pure player
  delete (Handle conn) pid = execute conn deletePlayerQuery (Only pid)
  view handle pid = viewRow_ selectPlayerByIdQuery handle pid
  set (Handle conn) player = do
    let params = [ ":id" := _playerPlayerId player
                 , ":accountId" := _playerAccountId player
                 , ":name" := _playerName player
                 , ":desc" := _playerDescription player
                 --, ":invId" := _playerInventory player
                 ]
    executeNamed conn setPlayerQuery params
    pure (Just player)

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
    results <- query conn selectPlayerQuery (Only name)
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

createInventory' :: Handle -> OwnerId -> IO (Maybe InventoryId)
createInventory' h@(Handle conn) = \case
  PlayerOwned uid -> do
    mplayer <- selectPlayerById h uid
    case mplayer of
      Just player -> do
        execute conn createInventoryQuery player
        inventoryId <- fromIntegral <$> lastInsertRowId conn
        pure $ Just $ InventoryId inventoryId
      Nothing -> pure Nothing
  RoomOwned uid -> undefined
  ItemOwned uid -> undefined
