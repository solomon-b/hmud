module Account where

--import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Trifecta (parseByteString)

import Parser
import Types ( ActiveUsers
             , Error(..)
             , User(..)
             , UserId
             )


-- This module is PURE functions related to player login, logout, and registration


-- I wish this worked:
--checkLogin' :: Connection -> Either Text Text -> Either Text Text -> IO (Either Text User)
--checkLogin' conn acc pass = do
--    acc' <- acc
--    pass' <- pass
--    eUser  <- selectUser conn acc'
--    user <- eUser
--    return $ checkPassword pass' user

-- DUPLICATE from state.hs, should be moved somewhere more general
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing = Left a

checkPassword :: Text -> User -> Either Error User
checkPassword pass user
    | pass /= userPassword user = Left InvalidPassword
    | otherwise = Right user

checkLogin :: [User] -> Text -> Either Error User
checkLogin = findUserByName

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
    

findUserByName :: [User] -> Text -> Either Error User
findUserByName users acc =
    maybeToEither NoSuchUser $ find (\user -> userUsername user == acc) users

validateUsername :: [User] -> ByteString -> Either Error Text
validateUsername users usernameBS = do
    let parsedUsername = resultToEither $ parseByteString word mempty usernameBS
    case parsedUsername of
        Left err -> Left . BadParse $ show err
        Right username -> do
            let eUser = findUserByName users (T.strip username)
            case eUser of
                Right _ -> Left NoSuchUser
                Left _ -> Right username

userIsLoggedIn :: ActiveUsers -> UserId -> Bool
userIsLoggedIn activeUsers userId =
    case M.lookup userId activeUsers of
        Just _ -> True
        Nothing -> False
