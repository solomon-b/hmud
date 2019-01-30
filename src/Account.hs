{-# LANGUAGE OverloadedStrings #-}
module Account where

--import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection)
import Text.Trifecta (parseByteString)

import Dispatch
import Parser
import SqliteLib
import State
import Types ( Error(..)
             , GlobalState(..)
             , ThreadEnv(..)
             , User(..)
             , UserId
             )


-- This module should be PURE functions related to player login, logout, and registration


-- I wish this worked:
--checkLogin' :: Connection -> Either Text Text -> Either Text Text -> IO (Either Text User)
--checkLogin' conn acc pass = do
--    acc' <- acc
--    pass' <- pass
--    eUser  <- selectUser conn acc'
--    user <- eUser
--    return $ checkPassword pass' user



checkPassword :: Text -> User -> Either Error User
checkPassword pass acc
    | pass /= userPassword acc = Left InvalidPassword
    | otherwise = Right acc

checkLogin :: Connection -> Either Error Text -> Either Error Text -> IO (Either Error User)
checkLogin _ (Left err') _ = print err' >> return (Left NoSuchUser)
checkLogin _ _ (Left err') = print err' >> return (Left InvalidPassword)
checkLogin conn (Right acc) (Right pass) = do

    eUser <- selectUser conn acc 
    return $ eUser >>= checkPassword pass 

-- TODO: Add minimum password strength req
validatePassword :: ByteString -> ByteString -> Either Error Text
validatePassword pass1BS pass2BS = do
    let parsedPass1 = resultToEither $ parseByteString word mempty pass1BS
    let parsedPass2 = resultToEither $ parseByteString word mempty pass2BS
    case (parsedPass1, parsedPass2) of
        (Right pass1, Right pass2) | pass1 == pass2 -> Right pass1
                                   | otherwise -> Left PasswordsDontMatch
        (Right _, Left err2) -> Left . BadParse $ show err2
        (Left err1, _) -> Left . BadParse $ show err1
    
validateUsername :: ByteString -> ReaderT ThreadEnv IO (Either Error Text)
validateUsername usernameBS = do
    conn <- asks threadEnvConn
    let parsedUsername = resultToEither $ parseByteString word mempty usernameBS
    case parsedUsername of
        Left err -> return . Left . BadParse $ show err
        Right username -> do
            eUser <- liftIO $ selectUser conn (T.strip username)
            case eUser of
                Right _ -> return $ Left NoSuchUser
                Left _ -> return $ Right username

userIsLoggedIn :: UserId -> ReaderT ThreadEnv IO Bool
userIsLoggedIn userId = do
    curState <- readState
    let stateMap = globalActiveUsers curState
        mUser = M.lookup userId stateMap
    case mUser of
        Just _ -> do
            sendMsg "You are already logged in!"
            return False
        Nothing -> return True
