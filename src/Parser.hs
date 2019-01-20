module Parser (commandParser, resultToEither, runParse, runWordParse, word)  where

import Control.Applicative
import Data.ByteString (ByteString)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Trifecta

import Types (User(..), Command(..), Direction(..))


word :: Parser Text
word = token $ T.pack <$> some letter

genParser :: (String, Maybe String, Command) -> Parser Command
genParser (str, mStr, comm) = token $
    case mStr of
        Just mstr' -> do
            void $ symbol str <|> symbol mstr'
            return comm
        Nothing -> do
            void $ symbol str
            return comm

commands :: [(String, Maybe String, Command)]
commands = [ ("exit"      , Just "quit" , Exit)
           , ("shutdown"  , Nothing     , Shutdown)
           , ("register"  , Just "r"    , Register)
           , ("login"     , Just "l"    , Login)
           , ("logout"    , Nothing     , Logout)
           , ("whois"     , Nothing     , Whois)
           , ("look"      , Just "l"    , Look)
           , ("getUsers"  , Nothing     , GetUsers)
           , ("north"     , Just "n"    , Move N)
           , ("south"     , Just "s"    , Move S)
           , ("east"      , Just "e"    , Move E)
           , ("west"      , Just "w"    , Move W)
           , ("northeast" , Just "ne"   , Move NE)
           , ("northwest" , Just "nw"   , Move NW)
           , ("southeast" , Just "se"   , Move SE)
           , ("southwest" , Just "sw"   , Move SW)

           --, ("getUser"  , Nothing    , GetUser)
           --, ("addUser"  , Nothing    , AddUser)
           --, ("say"      , Nothing    , Say)
           --, ("echo"     , Echo       , Echo)
           ]

commandParsers :: [Parser Command]
commandParsers = parserGetUser
               : parserAddUser
               : parserSay
               : parserEcho
               : fmap genParser commands

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

parserEcho :: Parser Command
parserEcho = token $ do
    void $ symbol "echo"
    str <- anyChar `manyTill` (char '\r' <|> char '\n')
    return $ Echo (T.pack str)


commandParser :: Parser Command
commandParser = choice commandParsers


runParse :: ByteString -> Either Text Command
runParse = resultToEither . parseByteString commandParser mempty

runWordParse :: ByteString -> Either Text Text
runWordParse bs = resultToEither $ parseByteString word mempty bs
    
resultToEither :: Result a -> Either Text a
resultToEither (Failure err') = Left . T.pack $ show err'
resultToEither (Success a) = Right a
