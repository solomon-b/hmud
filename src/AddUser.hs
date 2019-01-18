{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent
import Control.Exception
import Data.Text (Text, pack)
import Data.Typeable
import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import System.Environment (getArgs)

data User =
    User {
        userId        :: Integer
      , username      :: Text
      , shell         :: Text
      , homeDirectory :: Text
      , realName      :: Text
      , phone         :: Text
    } deriving (Eq, Show)

instance FromRow User where
    fromRow = User <$> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field

instance ToRow User where
    toRow (User _ username shell homeDirectory realName phone) =
        toRow (SQLNull, username, shell, homeDirectory, realName, phone)

data DuplicateData =
    DuplicateData deriving (Eq, Show, Typeable)

instance Exception DuplicateData

insertUser :: Query
insertUser =
    "INSERT INTO users\
    \ VALUES (?, ?, ?, ?, ?, ?)"


updateUser :: Query
updateUser = "UPDATE users SET username=?, shell=?, homeDirectory=?, realName=?, phone=? where id=?;"

getUserQuery :: Query
getUserQuery =
    "SELECT * from users where username = ?"

allUsers :: Query
allUsers =
    "SELECT * from users"

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
    results <- query conn getUserQuery (Only username)
    case results of
        [] -> return $ Nothing
        [user] -> return $ Just user
        _ -> throwIO DuplicateData

getUsers :: Connection -> IO ([User])
getUsers dbConn = do
    rows <- query_ dbConn allUsers
    return rows

addUser :: Connection -> [String] -> IO (Either String String)
addUser dbConn user = do
    case constructUser user of
        Nothing -> return $ Left "Please enter a valid user record"
        Just user' -> do
                execute dbConn insertUser user'
                return $ Right "insertion successful"
    
constructUser :: [String] -> Maybe User
constructUser xs =
    let f (a:b:c:d:e:[]) = Just $ User 0 a b c d e
        f _ = Nothing
    in f $ fmap pack xs

modifyUser :: Connection -> [String] -> IO (Either String String)
modifyUser dbConn user = do
    case constructUser (tail user) of
        Nothing -> return $ Left "Please enter a valid user record"
        Just _ -> do
                execute dbConn updateUser $ (tail user) ++ (pure $ head user)
                return $ Right "update successful"

selectCommand :: String -> Maybe (Connection -> [String] -> IO (Either String String))
selectCommand xs =
    case xs of
        "add" -> Just addUser
        "update" -> Just modifyUser
        _ -> Nothing


main :: IO ()
main = do
    conn <- open "finger.db"
    (command:input) <- getArgs
    let f = selectCommand command
    case f of
        Just f' -> do
            resp <- f' conn input
            case resp of
                Left err -> print err >> SQLite.close conn
                Right resp' -> do
                    putStrLn resp'
                    users <- getUsers conn
                    print users
                    SQLite.close conn
        Nothing -> print ("Please enter a valid command" :: String)
