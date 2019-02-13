{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module SqliteLib 
    ( Handle(..)
    , Config(..)
    , User(..)
    , UserId
    , withHandle
    , selectUser
    , formatUser
    , getUsersDb
    , getUserDb
    , addUserDb
    ) where

import Control.Exception (Exception, bracket, throwIO)
import Data.Text (Text, concat, pack)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Database.SQLite.Simple
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Text.RawString.QQ

import Errors


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
    toRow (User id_ username' password') = toRow (id_, username', password')

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


-----------------
---- Actions ----
-----------------

selectUser :: Handle -> Text -> IO (Either AppError User)
selectUser (Handle conn) user = do
    results <- query conn selectUserQuery (Only user)
    case results of
        [] -> return $ Left NoSuchUser
        [user'] -> return $ Right user'
        _ -> throwIO DuplicateData

insertUser :: Handle -> [Text] -> IO (Either Text User)
insertUser (Handle conn) user =
    case constructUser user of
        Left err -> return $ Left err
        Right user' -> do
                execute conn insertUserQuery user'
                return $ Right user'

addUserDb :: Handle -> User -> IO Text
addUserDb handle (User _ username password) = do
    eInserted <- insertUser handle [username, password]
    case eInserted of
        Left err' -> print err' >> return "Problem adding user"
        Right res -> return $ formatUser res

getUserDb :: Handle -> Text -> IO Text
getUserDb handle username = do
    eUser <- selectUser handle (T.strip username)
    case eUser of
        Left err' -> print err' >> return "Problem finding user"
        Right user' -> return $ formatUser user'

getUsersDb :: Handle -> IO [User]
getUsersDb (Handle conn) = query_ conn selectUsersQuery
