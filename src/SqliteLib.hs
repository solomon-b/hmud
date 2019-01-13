{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module SqliteLib 
    ( createDatabase
    , selectUser
    , insertUser
    , formatUser
    , constructUser
    , selectUsersQuery
    ) where

import Control.Exception
import Data.Text (Text, concat)
import Database.SQLite.Simple
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Text.RawString.QQ

import Types (User(..), UserRow, DuplicateData(..))


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
    \ VALUES (?, ?, ?)"

selectUsersQuery :: Query
selectUsersQuery =
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
    users <- query_ conn selectUsersQuery
    mapM_ print (users :: [User])
    SQLite.close conn
    where defUser :: UserRow
          defUser = (Null, "solomon", "pass")

selectUser :: Connection -> Text -> IO (Either Text User)
selectUser conn user = do
    results <- query conn selectUserQuery (Only user)
    case results of
        [] -> return $ Left "No such user"
        [user'] -> return $ Right user'
        _ -> throwIO DuplicateData

insertUser :: Connection -> [Text] -> IO (Either Text User)
insertUser conn user =
    case constructUser user of
        Left err -> return $ Left err
        Right user' -> do
                execute conn insertUserQuery user'
                return $ Right user'

formatUser :: User -> Text
formatUser (User _ username' _) =
    Data.Text.concat ["Username: ", username']

constructUser :: [Text] -> Either Text User
constructUser xs =
    let f [username,password] = Right $ User 0 username password
        f _ = Left "Please enter a valid user record"
    in f xs
