module HMud.Prompts where

import Control.Concurrent.STM
import Control.Monad.Except
import Control.Monad.Reader
import Data.List (find)
import qualified Data.Text as T
import Data.Text (Text)

import HMud.Commands
import HMud.Dispatch
import HMud.Errors
import HMud.Types
import HMud.Types.Classes

mainMenuPrompt ::
  ( MonadReader UserEnv m
  , MonadThread m
  , MonadTChan m
  , MonadPlayer m
  , MonadDB m
  , MonadError AppError m
  , MonadTCP m
  , MonadIO m
  , MonadGameState m
  ) => m Response
mainMenuPrompt = do
  respTChan <- asks userEnvRespTchan
  mapM_ (writeChannel respTChan . RespAnnounce) ["Welcome to hMud", "Options: register, login, exit"]
  resp <- prompt $ PromptEnv "> " False
  case resp of
    Exit -> do
      socket <- asks userEnvHandle
      threadId <- getThread
      pure $ RespExit threadId socket
    Login -> loginPrompt
    Register -> registerPrompt
    _ -> throwError $ InputErr InvalidCommand

loginPrompt ::
  ( MonadReader UserEnv m
  , MonadDB m
  , MonadTChan m
  , MonadPlayer m
  , MonadError AppError m
  , MonadTCP m
  , MonadIO m
  , MonadGameState m
  ) => m Response
loginPrompt = do
  active <- _gsActiveAccounts <$> readState
  liftIO $ print active
  throwIfThreadInActiveSession
  users    <- selectAllAccounts =<< asks getConnectionHandle
  username <- guardWord =<< prompt (PromptEnv "Login: " False)
  password <- guardWord =<< prompt (PromptEnv "Password: " True)
  account  <- verifyCredentials username password users
  throwIfAccountInActiveSession account
  login account

verifyCredentials :: MonadError AppError m => Text -> Text -> [Account] -> m Account
verifyCredentials email password accounts = do
      user <- maybe (throwError $ SessionErr EmailNotRegistered)
                    pure
                    (find (\account -> _accountEmail account == email) accounts)
      if password == _accountPassword user
        then pure user
        else throwError $ SessionErr InvalidPassword

registerPrompt ::
  ( MonadReader UserEnv m
  , MonadDB m
  , MonadError AppError m
  , MonadTChan m
  , MonadTCP m
  ) => m Response
registerPrompt = do
  handle    <- asks getConnectionHandle
  users     <- selectAllAccounts handle

  email     <- guardWord =<< prompt (PromptEnv "Email: " False)
  password1 <- guardWord =<< prompt (PromptEnv "Password: " True)
  password2 <- guardWord =<< prompt (PromptEnv "Repeat Password: " True)
  if not $ isEmailRegistered users email
    then
      if password1 == password2
        then do
          void $ insertAccount handle (Account (AccountId 0) email password1 Nothing)
          pure $ RespRegister email
        else throwError $ SessionErr PasswordsDontMatch
    else throwError $ SessionErr EmailAlreadyRegistered
  where
    isEmailRegistered :: [Account] -> Text -> Bool
    isEmailRegistered users email =
      let emails' = _accountEmail <$> users
          email' = T.strip email
      in case find (== email') emails' of
        Just _ -> True
        Nothing -> False

createPlayerPrompt ::
  ( MonadReader UserEnv m
  , MonadDB m
  , MonadTChan m
  , MonadTCP m
  , MonadError AppError m
  , MonadPlayer m
  ) => m Response
createPlayerPrompt = do
  userId <- getAccountId
  name <- guardWord =<< prompt (PromptEnv "Name: " False)
  description <- guardWord =<< prompt (PromptEnv "Description: " False)
  createPlayer userId name description
  pure $ RespPlayerCreated name

createPlayer ::
  ( MonadReader UserEnv m
  , MonadDB m
  ) => AccountId -> Text -> Text -> m ()
createPlayer uid name desc = do
  handle <- asks getConnectionHandle
  void . insertPlayer handle $ Player (PlayerId 0) uid name desc (pure $ InventoryId 0)

data PromptEnv = PromptEnv { _prefix :: Text, _suppressed :: Bool}

prompt ::
  ( MonadReader UserEnv m
  , MonadError AppError m
  , MonadTChan m
  , MonadTCP m
  ) => PromptEnv -> m Command
prompt (PromptEnv prefix suppressed) = do
  respTChan <- asks userEnvRespTchan
  cmdTchan  <- asks userEnvCmdTChan
  writeChannel respTChan $ Prompt prefix
  if suppressed
    then suppressEcho *> readCmd cmdTchan <* unsuppressEcho
    else readCmd cmdTchan

gamePrompt ::
  forall m.
  ( MonadReader UserEnv m
  , MonadTChan m
  , MonadError AppError m
  , MonadDB m
  , MonadPlayer m
  , MonadGameState m
  , MonadThread m
  ) => m (Response)
gamePrompt = do
  cmdTchan  <- asks userEnvCmdTChan
  respTChan <- asks userEnvRespTchan
  writeChannel respTChan $ Prompt "> "
  resp      <- readChannel cmdTchan
  case resp of
    Right cmd -> execCommand cmd
    Left err  -> throwError err

readCmd ::
  ( MonadTChan m
  , MonadError AppError m
  ) => TChan (Either AppError Command) -> m Command
readCmd tchan = do
  resp <- readChannel tchan
  case resp of
    Right cmd -> return cmd
    Left err -> throwError err

guardWord :: MonadError AppError m => Command -> m Text
guardWord (Word x) = pure x
guardWord _ = throwError $ InputErr InvalidCommand
