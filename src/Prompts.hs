module Hmud.Prompts where
-- This module will hold all command prompts
-- NOT CURRENTLY IN USE


mainMenuPrompt :: ReaderT ThreadEnv IO ()
mainMenuPrompt = undefined

loginPrompt :: ReaderT ThreadEnv IO ()
loginPrompt = undefined

usernameRegPrompt :: ReaderT ThreadEnv IO (Either Error Text)
usernameRegPrompt = undefined

passwordRegPrompt :: ReaderT ThreadEnv IO (Either Error Text)
passwordRegPrompt = undefined

registerPrompt :: ReaderT ThreadEnv IO ()
registerPrompt = undefined

gamePrompt :: (User, ThreadId) -> ReaderT ThreadEnv IO ()
gamePrompt (user, _) = undefined
