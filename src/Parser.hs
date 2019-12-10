{-# LANGUAGE FlexibleContexts #-}
module Parser ( Command(..)
              , Direction(..)
              , commandParser
              , mainMenuParser
              , resultToEither
              , runParse
              , runParseRaw
              , runMainMenuParse
              , runWordParse
              , word
              , testParse
              )  where

import Control.Applicative
import Data.ByteString (ByteString)
import Control.Monad (void)
import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T
import Text.Trifecta

import Errors
import SqliteLib (User(..))

---------------
---- Types ----
---------------

data Command
    = GetUsers
    | GetUser Text
    | AddUser User
    | Echo Text
    | Exit
    | Shutdown
    | Register
    | Raw Text
    | Look
    | Login
    | Logout
    | Whois
    | Say Text
    | Move Direction
    | Word Text
    | SuppressEcho
    | UnsuppressEcho
    deriving (Eq, Show)

data Direction =
    N | S | E | W | NW | NE | SW | SE | U | D deriving (Eq, Ord)

instance Show Direction where
    show U  = "Up"
    show D  = "Down"
    show N  = "North"
    show S  = "South"
    show E  = "East"
    show W  = "West"
    show NW = "Northwest"
    show NE = "Northeast"
    show SW = "Southwest"
    show SE = "Southeast"


----------------
---- Parser ----
----------------

word :: Parser Text
word = token $ T.pack <$> some letter

genParser :: (String, Maybe String, Command) -> Parser Command
genParser (str, mStr, comm) = token $
    case mStr of
        Just mstr' -> do
            void $ (symbol str <* eof) <|> (symbol mstr' <* eof)
            return comm
        Nothing -> do
            void $ symbol str
            eof
            return comm

mainMenuCommands :: [(String, Maybe String, Command)]
mainMenuCommands =
             [ ("exit"      , Just "quit" , Exit)
             , ("register"  , Just "r"    , Register)
             , ("login"     , Just "l"    , Login)
             ]

userCommands :: [(String, Maybe String, Command)]
userCommands =
             [ ("exit"      , Just "quit" , Exit)
             , ("logout"    , Nothing     , Logout)
             , ("whois"     , Nothing     , Whois)
             , ("look"      , Just "l"    , Look)
             , ("north"     , Just "n"    , Move N)
             , ("south"     , Just "s"    , Move S)
             , ("east"      , Just "e"    , Move E)
             , ("west"      , Just "w"    , Move W)
             , ("northeast" , Just "ne"   , Move NE)
             , ("northwest" , Just "nw"   , Move NW)
             , ("southeast" , Just "se"   , Move SE)
             , ("southwest" , Just "sw"   , Move SW)
             --, ("getUser"  , Nothing    , GetUser)
             --, ("say"      , Nothing    , Say)
             --, ("echo"     , Echo       , Echo)
             ]

adminCommands :: [(String, Maybe String, Command)]
adminCommands =
              [ ("getUsers"  , Nothing     , GetUsers)
              , ("shutdown"  , Nothing     , Shutdown)
            --, ("addUser"  , Nothing    , AddUser)
              ]

commandParsers :: [Parser Command]
commandParsers = parserGetUser
               : parserAddUser
               : parserSay
               : fmap genParser (userCommands ++ adminCommands)

mainMenuParsers :: [Parser Command]
mainMenuParsers = fmap genParser mainMenuCommands

parserGetUser :: Parser Command
parserGetUser = token $ do
    void $ string "getUser"
    void $ char ' '
    username' <- word
    return $ GetUser username'

parserAddUser :: Parser Command
parserAddUser = token $ do
    void $ string "addUser"
    void $ char ' '
    username' <- word
    password' <- word
    return $ AddUser (User 0 username' password')

parserSay :: Parser Command
parserSay = token $ do
    void $ symbol "say"
    str <- anyChar `manyTill` (char '\r' <|> char '\n')
    return $ Say (T.pack str)

--sendHandle' sock $ BS.pack [255,251,1]
--("\255\\\251\\\SOH", Nothing   , SuppressEcho)
parserSuppress :: Parser Command
parserSuppress = token $ do
    void $ char '3'
    eof
    return SuppressEcho

parserRaw :: Parser Command
parserRaw = Raw <$> word

testParse :: ByteString -> Result Command
testParse bs = parseByteString parserSuppress mempty bs

commandParser :: Parser Command
commandParser = choice commandParsers

mainMenuParser :: Parser Command
mainMenuParser = choice mainMenuParsers

runParse' :: MonadError AppError m => Parser a -> ByteString -> m a
runParse' parser bs =
    let parse = parseByteString parser mempty bs
    in case parse of
        Success cmd -> return cmd
        Failure _   -> throwError InvalidCommand

runParse :: MonadError AppError m => ByteString -> m Command
runParse = runParse' (try commandParser <|> try mainMenuParser)

runParseRaw :: MonadError AppError m => ByteString -> m Command
runParseRaw = runParse' parserRaw

runMainMenuParse :: MonadError AppError m => ByteString -> m Command
runMainMenuParse = runParse' mainMenuParser

runWordParse :: MonadError AppError m => ByteString -> m Text
runWordParse = runParse' word

resultToEither :: Result a -> Either AppError a
resultToEither (Failure _) = Left InvalidCommand
resultToEither (Success a) = Right a
