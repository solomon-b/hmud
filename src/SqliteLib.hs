{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module SqliteLib 
    ( createDatabase
    , selectUser
    , insertUser
    , formatUser
    , constructUser
    , selectUsersQuery
    , getUsersDb
    , getUserDb
    , addUserDb
    ) where

import Control.Exception
import Data.List (intersperse)
import Data.Text (Text, concat, pack)
import qualified Data.Text as T
import Database.SQLite.Simple hiding (Error)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Text.RawString.QQ

import Types (Error(..), User(..), UserRow, DuplicateData(..))


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

selectUser :: Connection -> Text -> IO (Either Error User)
selectUser conn user = do
    results <- query conn selectUserQuery (Only user)
    case results of
        [] -> return $ Left NoSuchUser
        [user'] -> return $ Right user'
        _ -> throwIO DuplicateData

insertUser :: Connection -> [Text] -> IO (Either Text User)
insertUser conn user =
    case constructUser user of
        Left err -> return $ Left err
        Right user' -> do
                execute conn insertUserQuery user'
                return $ Right user'

-- TODO: Thse pure functions should probably go somewhere else?
formatUser :: User -> Text
formatUser (User uid name _) =
    Data.Text.concat [ "Player: "  , name, "\t"
                     , "UID: "     , (pack . show) uid]

constructUser :: [Text] -> Either Text User
constructUser xs =
    let f [username,password] = Right $ User 0 username password
        f _ = Left "Please enter a valid user record"
    in f xs


-----------------------------
---- Getters and Setters ----
-----------------------------

addUserDb :: Connection -> User -> IO Text
addUserDb conn (User _ username password) = do
    eInserted <- insertUser conn [username, password]
    case eInserted of
        Left err' -> print err' >> return "Problem adding user"
        Right res -> return $ formatUser res

getUserDb :: Connection -> Text -> IO Text
getUserDb conn username = do
    eUser <- selectUser conn (T.strip username)
    case eUser of
        Left err' -> print err' >> return "Problem finding user"
        Right user' -> return $ formatUser user'

getUsersDb :: Connection -> IO Text
getUsersDb conn = do
    rows <- query_ conn selectUsersQuery
    let usernames = userUsername <$> rows
        newlineSeperated =
            T.concat $ intersperse "\n" usernames ++ pure (T.pack "\r\n")
    return newlineSeperated
