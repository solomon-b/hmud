{-# LANGUAGE QuasiQuotes #-}
module HMud.SqliteLib
    ( Handle(..)
    , Config(..)
    , User(..)
    , UserId
    , withHandle
    , insertUser
    , selectUser
    , selectAllUsers
    , formatUser
    ) where

import Control.Exception (Exception, bracket, throwIO)
import Data.Text (Text, concat, pack)
import Data.Typeable (Typeable)
import Database.SQLite.Simple
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Text.RawString.QQ

import HMud.Errors


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
    toRow (User _ username' password') = toRow (username', password')

instance Show User where
    show user = show (userUsername user)

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
insertUserQuery =
    "INSERT INTO users\
    \ VALUES (NULL, ?, ?)"

selectAllUsersQuery :: Query
selectAllUsersQuery =
    "SELECT * from users"

selectUserQuery :: Query
selectUserQuery =
    "SELECT * from users where username = ?"


-----------------------
---- Database CRUD ----
-----------------------

createDatabase :: IO ()
createDatabase = do
    conn <-  open "hmud.db"
    execute_ conn createUserTableQuery
    execute conn insertUserQuery defUser
    users <- query_ conn selectAllUsersQuery
    mapM_ print (users :: [User])
    SQLite.close conn
    where defUser :: UserRow
          defUser = (Null, "solomon", "pass")

-- TODO: Thse pure functions should probably go somewhere else?
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
