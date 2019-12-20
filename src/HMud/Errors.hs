module HMud.Errors where

--import Data.Text (Text)
import Control.Exception

--TODO: Break up errors into types based on usage class and learn how to
--      unify them where necessary.
--TODO: Create TextShow instances for errors that get sent to the client.

data SessionError
  = NoActiveSession
  | ActiveSession
  | EmailAlreadyRegistered
  | EmailNotRegistered
  | PasswordsDontMatch
  | InvalidPassword
  | NoAccountPlayer
  | AccountAlreadyActive
  deriving Show

data InputError
  = InvalidCommand
  | NoCommand
  deriving Show

data GameError
  = NoSuchDirection
  | NoSuchItem
  | NoSuchPlayer
  | NoSuchRoom
  deriving Show

data DbError
  = DbConnectionError
  | NoRow
  deriving Show

data ParserError
  = BadParse
  deriving Show

data AppError
  = GameErr GameError
  | InputErr InputError
  | SessionErr SessionError
  | DbErr DbError
  | ParseErr ParserError
  deriving Show

--data AppError
--  = NoSuchAccount
--  | NoSuchUser         -- ???
--  | NoSuchRoom         -- In Game
--  | NotLoggedIn        -- Auth
--  | AlreadyLoggedIn    -- Auth
--  | UserNotFound       -- Auth
--  | InvalidPassword    -- Auth
--  | PasswordsDontMatch -- Auth
--  | UserNotInPlayerMap -- System?
--  | InvalidCommand     -- In Game
--  | NoSuchObject Text
--  | IgnoredResponse    -- Response codes passed back by the telnet client I don't care about.
--  | PlayerNameAlreadyExists
--  | EmailAlreadyRegistered
--  | AccountHasNoPlayer
--  | DataBaseError DbError

--instance Show AppError where
--  show = \case
--    NoSuchAccount           -> "There is no such account."
--    NoSuchUser              -> "There is no such user."
--    NoSuchRoom              -> "There is no room in that direction."
--    NotLoggedIn             -> "You are not logged in."
--    AlreadyLoggedIn         -> "You are already logged in."
--    UserNotFound            -> "No user found in database."
--    InvalidPassword         -> "Invalid Password."
--    PasswordsDontMatch      -> "Your password entries don not match."
--    UserNotInPlayerMap      -> "User not found in game."
--    InvalidCommand          -> "Please enter a valid command."
--    IgnoredResponse         -> "Telnet Client response we don't care about."
--    PlayerNameAlreadyExists -> "Player name is taken, try another one."
--    EmailAlreadyRegistered  -> "Email address is already registered."
--    NoSuchObject name       -> "There is no " ++ show name ++ " here."
--    AccountHasNoPlayer      -> "There is no character associated with this account"

instance Exception AppError
