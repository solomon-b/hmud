module HMud.Prompts where

import Control.Concurrent.STM
import Control.Monad.Fail
import Control.Monad.Except
import Control.Monad.Reader
import Data.List (find)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text (Text)

import Control.Lens

import HMud.Account
import HMud.Commands
import HMud.Dispatch
import HMud.Errors
import HMud.Types
import HMud.Types.Classes

mainMenuPrompt ::
  ( MonadReader UserEnv m
  , MonadThread m
  , MonadGameState m
  , MonadTChan m
  , MonadPlayer m
  , MonadDB m
  , MonadError AppError m
  , MonadTCP m
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
    _ -> throwError InvalidCommand

loginPrompt ::
  ( MonadReader UserEnv m
  , MonadDB m
  , MonadGameState m
  , MonadTChan m
  , MonadPlayer m
  , MonadError AppError m
  , MonadTCP m
  ) => m Response
loginPrompt = do
  gs <- readState
  handle <- asks getConnectionHandle
  users <- selectAllUsers handle

  username <- prompt (PromptEnv "Login: " False)
  password <- prompt (PromptEnv "Password: " True)

  let loginResult = verifyLogin username password users
  case loginResult of
    Left e -> throwError e
    Right user -> do
      playerId <- gs ^. gsUserMap . at (_userUserId user) . to (maybe (throwError NoSuchUser) pure)
      player <- gs ^. gsActivePlayers . at playerId . to (maybe (throwError NoSuchUser) pure)
      if playerId `M.member` (gs ^. gsActivePlayers)
        then loginPrompt
        else do
        setUser  $ _userUserId user
        setState $ gs & gsActivePlayers . at playerId ?~ player
        setState $ gs & gsPlayerMap . at (RoomId 1) . non mempty <>~ pure playerId
        pure . RespAnnounce $ _userUsername user `T.append` " Logged In"

verifyLogin :: MonadError AppError m => Command -> Command -> [User] -> m User
verifyLogin (Word username) (Word password) users = do
      user <- maybe (throwError NoSuchUser) pure $ find (\user -> _userUsername user == username) users
      if password == (_userPassword user)
        then pure user
        else throwError InvalidPassword
verifyLogin _ _ _ = throwError InvalidCommand

registerPrompt ::
  ( MonadReader UserEnv m
  , MonadDB m
  , MonadGameState m
  , MonadPlayer m
  , MonadError AppError m
  , MonadTChan m
  , MonadTCP m
  ) => m Response
registerPrompt = do
  handle <- asks getConnectionHandle
  users <- selectAllUsers handle

  username' <- prompt (PromptEnv "Login: " False)
  password1' <- prompt (PromptEnv "Password: " True)
  password2' <- prompt (PromptEnv "Repeat Password: " True)

  case (username', password1', password2') of
    (Word username, Word password1, Word password2) ->
      if not $ usernameRegistered users username
        then
          if password1 == password2
            then do
              void $ insertUser handle (User 0 username password1)
              pure $ RespRegister username
            else throwError PasswordsDontMatch
        else throwError UsernameAlreadyExists
    _ -> registerPrompt

createPlayerPrompt ::
  ( MonadReader UserEnv m
  , MonadDB m
  , MonadTChan m
  , MonadTCP m
  , MonadError AppError m
  , MonadPlayer m
  , MonadFail m
  ) => m Response
createPlayerPrompt = do
  userId <- maybe (throwError NotLoggedIn ) pure =<< getUserId
  Word name <- prompt (PromptEnv "Name: " False)
  Word description <- prompt (PromptEnv "Description: " False)
  createPlayer userId name description
  pure $ RespPlayerCreated name

createPlayer ::
  ( MonadReader UserEnv m
  , MonadDB m
  ) => UserId -> Text -> Text -> m ()
createPlayer uid name desc = do
  handle <- asks getConnectionHandle
  void . insertPlayer handle $ Player (PlayerId 0) name uid desc (InventoryId 0)

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
  , MonadIO m
  , MonadTChan m
  ) => m (Either AppError Response)
gamePrompt = do
  env       <- ask
  cmdTchan  <- asks userEnvCmdTChan
  respTChan <- asks userEnvRespTchan
  writeChannel respTChan $ Prompt "> "
  resp      <- readChannel cmdTchan
  case resp of
    Right cmd -> runExceptT $ runReaderT (execCommand cmd) env
    Left err  -> return $ Left err

readCmd ::
  ( MonadTChan m
  , MonadError AppError m
  ) => TChan (Either AppError Command) -> m Command
readCmd tchan = do
  resp <- readChannel tchan
  case resp of
    Right cmd -> return cmd
    Left err -> throwError err
