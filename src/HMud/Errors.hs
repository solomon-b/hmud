module HMud.Errors where

import Data.Text (Text)
import Control.Exception

--TODO: Identify use cases for all errors and how they should be handled.
--TODO: Break up errors into types based on usage class and learn how to
--      unify them where necessary.
--TODO: Create TextShow instances for errors that get sent to the client.
data AppError
    = NoSuchUser         -- ???
    | NoSuchRoom         -- In Game
    | NotLoggedIn        -- Auth
    | AlreadyLoggedIn    -- Auth
    | UserNotFound       -- Auth
    | InvalidPassword    -- Auth
    | PasswordsDontMatch -- Auth
    | UserNotInPlayerMap -- System?
    | InvalidCommand     -- In Game
    | NoSuchObject Text
    | IgnoredResponse    -- Response codes passed back by the telnet client I don't care about.
    | UsernameAlreadyExists

instance Show AppError where
  show = \case
    NoSuchUser            -> "There is no such user."
    NoSuchRoom            -> "There is no room in that direction."
    NotLoggedIn           -> "You are not logged in."
    AlreadyLoggedIn       -> "You are already logged in."
    UserNotFound          -> "No user found in database."
    InvalidPassword       -> "Invalid Password."
    PasswordsDontMatch    -> "Your password entries don not match."
    UserNotInPlayerMap    -> "User not found in game."
    InvalidCommand        -> "Please enter a valid command."
    IgnoredResponse       -> "Telnet Client response we don't care about."
    UsernameAlreadyExists -> "Username is taken, try another one."
    NoSuchObject name     -> "There is no " ++ show name ++ " here."

instance Exception AppError
