module HMud.Dispatch where

{-
Message Dispatching to/from the telnet client.
-}


import Control.Concurrent (killThread)
import Control.Concurrent.Async
import Control.Concurrent.STM (TChan)
import Control.Monad (forever)
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (MonadState, execStateT, put, get)
import Data.ByteString (ByteString)
import Data.ByteString as BS (pack)
import Data.Text.Encoding (encodeUtf8)

import HMud.Errors
import HMud.Parser.Commands
import HMud.Socket as Socket
import HMud.TelnetLib
import HMud.Types
    ( UserEnv(..)
    , HasSocketHandle(..)
    , MonadTChan(..)
    , MonadTCP(..)
    , Response(..)
    , tshow
    )

-- Always define the naturals:
data Peano = Zero | Succ Peano deriving Show             -- Temporary for debug prints in `cmdLoopInner`
data ParseState = Normal | Multiline Peano deriving Show -- Temporary for debug prints in `cmdLoopInner`

isZero :: Peano -> Bool
isZero Zero = True
isZero _    = False

decr :: Peano -> Peano
decr Zero = Zero
decr (Succ p) = p

suppressEcho ::
    ( MonadReader UserEnv m
    , MonadTCP m
    ) => m ()
suppressEcho = do
    sock <- asks getSocketHandle
    sendHandle' sock $ BS.pack [255,251,1]

unsuppressEcho ::
    ( MonadReader UserEnv m
    , MonadTCP m
    ) => m ()
unsuppressEcho = do
    sock <- asks getSocketHandle
    sendHandle' sock $ BS.pack [255,252,1]

cmdLoopInner ::
  ( MonadTChan m
  , MonadTCP m
  , MonadState ParseState m
  ) => Socket.Handle -> TChan (Either AppError Command) -> m ()
cmdLoopInner handle cmdChan = forever $ do
  s      <- get
  rawMsg <- readHandle' handle
  let cmds = runExcept $ parseRawMsg s rawMsg
  case cmds of
    Right (cmd, s')      -> writeChannel cmdChan (Right cmd) >> put s'
    Left IgnoredResponse -> return ()
    Left e               -> writeChannel cmdChan (Left e)
  where
    parseRawMsg :: ParseState -> ByteString -> Except AppError (Command, ParseState)
    parseRawMsg state rawMsg =
      case (unBuffer . processStream) rawMsg of
        Nothing  -> throwError IgnoredResponse
        Just msg ->
          case state of
            Normal -> do
              cmd <- runParse msg
              case cmd of
                Login -> return (Login, Multiline (Succ Zero))
                Register -> return (Register, Multiline (Succ $ Succ Zero))
                _ ->  return (cmd, Normal)
            Multiline cs -> do
              str <- runWordParse rawMsg
              if isZero cs
              then return (Word str, Normal)
              else return (Word str, Multiline $ decr cs)

dispatchLoop ::
  forall m.
  (MonadIO m
  --( MonadTChan m
  --, MonadTCP m
  ) => Socket.Handle -> TChan (Either AppError Command) -> TChan Response -> TChan Response -> m ()
dispatchLoop handle cmdChan respChan publicChan =
  let
    respLoop :: IO ()
    respLoop = forever $ do
      resp <- readChannel respChan
      case resp of
        RespExit threadId sock -> do
          putStrLn $ "Closing thread: " ++ show threadId
          writeChannel respChan $ RespAnnounce "Goodbye!"
          closeHandle' sock
          killThread threadId
        _ -> sendHandle' handle . encodeUtf8 $ tshow resp
    cmdLoop :: IO ()
    cmdLoop = liftIO . void $ execStateT (cmdLoopInner handle cmdChan) Normal
    publicLoop :: IO ()
    publicLoop = forever $ do
      public  <- readChannel publicChan
      sendHandle' handle . encodeUtf8 $ tshow public
  in liftIO $ race_ respLoop (race_ cmdLoop publicLoop)
