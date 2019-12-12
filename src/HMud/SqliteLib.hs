{-# LANGUAGE QuasiQuotes #-}
module HMud.SqliteLib
    ( Handle(..)
    , Config(..)
    , User(..)
    , UserId
    , withHandle
    , createDatabase
    , insertUser
    , selectUser
    , selectAllUsers
    , formatUser
    , insertPlayer
    , selectPlayer
    , selectAllPlayers
    ) where

import Control.Exception (Exception, bracket, throwIO)
import Data.Text (Text, concat, pack)
import Data.Typeable (Typeable)
import Database.SQLite.Simple
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Text.RawString.QQ

import HMud.Errors
import HMud.Types


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

type UserRow = (Null, Text, Text)

data DuplicateData = DuplicateData
    deriving (Eq, Show, Typeable)

instance Exception DuplicateData

-----------------
---- Queries ----
-----------------

createUserTableQuery :: Query
createUserTableQuery = [r|
CREATE TABLE IF NOT EXISTS users
    (id INTEGER PRIMARY KEY AUTOINCREMENT,
     username TEXT UNIQUE,
     password TEXT)
|]

insertUserQuery :: Query
insertUserQuery = [r|
INSERT INTO users
 VALUES (NULL, ?, ?)
|]

selectAllUsersQuery :: Query
selectAllUsersQuery = "SELECT * from users"

selectUserQuery :: Query
selectUserQuery = "SELECT * from users where username = ?"

createPlayerTableQuery :: Query
createPlayerTableQuery = [r|
CREATE TABLE IF NOT EXISTS players
    (id INTEGER PRIMARY KEY AUTOINCREMENT,
     name TEXT UNIQUE,
     userId INTEGER,
     description TEXT,
     inventoryId INTEGER AUTOINCREMENT)
|]

insertPlayerQuery :: Query
insertPlayerQuery = [r|
INSERT INTO players
 VALUES (NULL, ?, ?, ?, NULL)
|]

selectAllPlayersQuery :: Query
selectAllPlayersQuery = "SELECT * from players"

selectPlayerQuery :: Query
selectPlayerQuery = "SELECT * from players where name = ?"

-----------------------
---- Database CRUD ----
-----------------------

createDatabase :: IO ()
createDatabase = do
    conn <-  open "hmud.db"
    execute_ conn createUserTableQuery
    execute_ conn createPlayerTableQuery
    --execute conn insertUserQuery defUser
    users <- query_ conn selectAllUsersQuery
    mapM_ print (users :: [User])
    SQLite.close conn
    --where defUser :: UserRow
    --      defUser = (Null, "solomon", "pass")

-- TODO: These pure functions should probably go somewhere else?
formatUser :: User -> Text
formatUser (User uid name _) =
    Data.Text.concat [ "Player: "  , name, "\t"
                     , "UID: "     , (pack . show) uid]

-----------------
---- Actions ----
-----------------
-- TODO: Add additional User DB CRUD actions

selectUser :: Handle -> Text -> IO (Either AppError User)
selectUser (Handle conn) user = do
    results <- query conn selectUserQuery (Only user)
    case results of
        [] -> return $ Left NoSuchUser
        [user'] -> return $ Right user'
        _ -> throwIO DuplicateData

selectAllUsers :: Handle -> IO [User]
selectAllUsers (Handle conn) = query_ conn selectAllUsersQuery

insertUser :: Handle -> User -> IO User
insertUser (Handle conn) user = do
    execute conn insertUserQuery user
    return user

selectPlayer :: Handle -> Text -> IO (Either AppError Player)
selectPlayer (Handle conn) user = do
    results <- query conn selectPlayerQuery (Only user)
    case results of
        [] -> return $ Left NoSuchUser -- TODO: Add Player Specific Error Message
        [user'] -> return $ Right user'
        _ -> throwIO DuplicateData

selectAllPlayers :: Handle -> IO [Player]
selectAllPlayers (Handle conn) = query_ conn selectAllPlayersQuery

insertPlayer :: Handle -> Player -> IO Player
insertPlayer (Handle conn) user = do
    execute conn insertPlayerQuery user
    return user
