{-# LANGUAGE QuasiQuotes                #-}
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
    ) where

import Control.Exception (Exception, bracket, throwIO)
import Data.Text (Text, concat, pack)
import Data.Typeable (Typeable)
import Database.SQLite.Simple
import qualified Database.SQLite.Simple as SQLite
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

selectAllAccountsQuery :: Query
selectAllAccountsQuery = "SELECT * from accounts"

selectAccountQuery :: Query
selectAccountQuery = "SELECT * from accounts where email = ?"

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

selectAllPlayersQuery :: Query
selectAllPlayersQuery = "SELECT * from players"

selectPlayerQuery :: Query
selectPlayerQuery = "SELECT * from players where name = ?"

selectPlayerByAccountIdQuery :: Query
selectPlayerByAccountIdQuery = "SELECT * from players where accountId = ?"

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

selectAccount :: Handle -> Text -> IO (Maybe Account)
selectAccount (Handle conn) name = do
    results <- query conn selectAccountQuery (Only name)
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

selectAllPlayers :: Handle -> IO [Player]
selectAllPlayers (Handle conn) = query_ conn selectAllPlayersQuery

insertPlayer :: Handle -> Player -> IO Player
insertPlayer (Handle conn) player = do
    execute conn insertPlayerQuery player
    return player
