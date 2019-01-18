{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module SqliteLib where

import Control.Exception
import Data.Text (Text, append, concat)
import Data.Typeable
import Database.SQLite.Simple hiding (close, bind)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Text.RawString.QQ


-----------------------------
---- Types and Instances ----
-----------------------------

data Account = 
    Account { getAccountId       :: Integer
            , getAccount         :: Text
            , getPassword        :: Text 
            } deriving (Eq, Show)

data User =
    User { getUserId        :: Integer
         , getUsername      :: Text
         , getShell         :: Text
         , getHomeDirectory :: Text
         , getRealName      :: Text
         , getPhone         :: Text
         } deriving (Eq, Show)

type UserRow = (Null, Text, Text, Text, Text, Text)
type AccountRow = (Null, Text, Text)

data DuplicateData = DuplicateData 
    deriving (Eq, Show, Typeable)

instance Exception DuplicateData

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
    toRow (User id_ username' shell' homeDir' realName' phone') =
        toRow (id_, username', shell', homeDir', realName', phone')

instance FromRow Account where
    fromRow = Account <$> field <*> field <*> field

instance ToRow Account where
    toRow (Account id_ username' password') = toRow (id_, username', password')


-----------------
---- Queries ----
-----------------

createUserTableQuery :: Query
createUserTableQuery = [r|
CREATE TABLE IF NOT EXISTS users
    (id INTEGER PRIMARY KEY AUTOINCREMENT,
     username TEXT UNIQUE,
     shell TEXT, homeDirectory TEXT,
     realName TEXT, phone TEXT)
|]

createAccountTableQuery :: Query
createAccountTableQuery = [r|
CREATE TABLE IF NOT EXISTS accounts
    (id INTEGER PRIMARY KEY AUTOINCREMENT,
     account TEXT UNIQUE,
     password TEXT)
|]

insertUserQuery :: Query
insertUserQuery =
    "INSERT INTO users\
    \ VALUES (?, ?, ?, ?, ?, ?)"

selectUsersQuery :: Query
selectUsersQuery =
    "SELECT * from users"

selectUserQuery :: Query
selectUserQuery =
    "SELECT * from users where username = ?"

insertAccountQuery :: Query
insertAccountQuery =
    "INSERT INTO accounts\
    \ VALUES (?, ?, ?)"

selectAccountQuery :: Query
selectAccountQuery =
    "SELECT * from accounts where account = ?"

selectAccountsQuery :: Query
selectAccountsQuery =
    "SELECT * from accounts"


-----------------------
---- Database CRUD ----
-----------------------

createDatabase :: IO ()
createDatabase = do
    conn <-  open "finger.db"
    execute_ conn createUserTableQuery
    execute_ conn createAccountTableQuery
    execute conn insertUserQuery defUser
    execute conn insertAccountQuery defAccount
    users <- query_ conn selectUsersQuery
    mapM_ print (users :: [User])
    accounts <- query_ conn selectAccountsQuery
    mapM_ print (accounts :: [Account])
    SQLite.close conn
    where defUser :: UserRow
          defUser = (Null, "callen", "/bin/zsh",
                   "/home/callen", "Chris Allen",
                   "555-123-4567")
          defAccount :: AccountRow
          defAccount = (Null, "solomon", "password")

selectAccount :: Connection -> Text -> IO (Either Text Account)
selectAccount conn account = do
    results <- query conn selectAccountQuery (Only account)
    case results of
        [] -> return $ Left "No such account"
        [account'] -> return $ Right account'
        _ -> throwIO DuplicateData

selectUser :: Connection -> Text -> IO (Either Text User)
selectUser conn username' = do
    results <- query conn selectUserQuery (Only username')
    case results of
        [] -> return $ Left $ append "No such user: " username'
        [user] -> return $ Right user
        _ -> throwIO DuplicateData

insertUser :: Connection -> [Text] -> IO (Either Text User)
insertUser conn user = do
    case constructUser user of
        Left err -> return $ Left err
        Right user' -> do
                execute conn insertUserQuery user'
                return $ Right user'

formatUser :: User -> Text
formatUser (User _ username' shell' homeDir' realName' _) =
    Data.Text.concat [ "Login: "    , username', "\t\t\t\t"
             , "Name: "     , realName', "\n"
             , "Directory: ", homeDir' , "\t\t\t"
             , "Shell: "    , shell']

constructUser :: [Text] -> Either Text User
constructUser xs =
    let f (a:b:c:d:e:[]) = Right $ User 0 a b c d e
        f _ = Left "Please enter a valid user record"
    in f xs
