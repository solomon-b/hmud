module HMud.Account where

import Control.Monad.Except
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T

import HMud.Parser.Commands
import HMud.Errors
import HMud.SqliteLib (User(..))
import HMud.Types

-- DUPLICATE from state.hs, should be moved somewhere more general
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing = Left a

usernameRegistered :: [User] -> Text -> Bool
usernameRegistered users user =
  let users' = _userUsername <$> users
      user' = T.strip user
  in case find (== user') users' of
    Just _ -> True
    Nothing -> False

usernameIsAvailable :: MonadError AppError m => [User] -> ByteString -> m Text
usernameIsAvailable users usernameBS = do
  username <- runWordParse usernameBS
  if usernameRegistered users username
    then throwError UsernameAlreadyExists
    else pure username

userIsLoggedIn :: ActivePlayers -> PlayerId -> Bool
userIsLoggedIn activeUsers playerId =
  case M.lookup playerId activeUsers of
    Just _ -> True
    Nothing -> False
