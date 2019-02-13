module Errors where

--TODO: Identify use cases for all errors and how they should be handled.
--TODO: Break up errors into types based on usage class and learn how to 
--      unify them where necessary.
--TODO: Create TextShow instances for errors that get sent to the client.
data AppError = NoSuchUser      -- ???
           | NoSuchRoom         -- In Game
           | NotLoggedIn        -- Auth
           | AlreadyLoggedIn    -- Auth
           | UserNotFound       -- Auth
           | InvalidPassword    -- Auth
           | PasswordsDontMatch -- Auth
           | UserNotInPlayerMap -- System?
           | InvalidCommand     -- In Game
           | BadParse String    -- Parser

instance Show AppError where
    show NoSuchUser         = "There is no such user."
    show NoSuchRoom         = "There is no room in that direction."
    show NotLoggedIn        = "You are not logged in."
    show AlreadyLoggedIn    = "You are already logged in."
    show UserNotFound       = "No user found in database."
    show InvalidPassword    = "Invalid Password."
    show PasswordsDontMatch = "Your password entries don not match."
    show UserNotInPlayerMap = "User not found in game."
    show InvalidCommand     = "Please enter a valid command."
    show (BadParse str)     = "Bad Parse: " ++ show str