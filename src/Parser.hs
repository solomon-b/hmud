module Parser ( commandParser
              , mainMenuParser
              , resultToEither
              , runParse
              , runMainMenuParse
              , runWordParse
              , word
              )  where

import Control.Applicative
import Data.ByteString (ByteString)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Trifecta

import Types (User(..), Command(..), Direction(..), Error(..))


word :: Parser Text
word = token $ T.pack <$> some letter

genParser :: (String, Maybe String, Command) -> Parser Command
genParser (str, mStr, comm) = token $
    case mStr of
        Just mstr' -> do
            void $ symbol str <|> (symbol mstr' <* eof)
            return comm
        Nothing -> do
            void $ symbol str
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
               : parserEcho
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

parserEcho :: Parser Command
parserEcho = token $ do
    void $ symbol "echo"
    str <- anyChar `manyTill` (char '\r' <|> char '\n')
    return $ Echo (T.pack str)


commandParser :: Parser Command
commandParser = choice commandParsers

mainMenuParser :: Parser Command
mainMenuParser = choice mainMenuParsers

runParse :: ByteString -> Either Error Command
runParse = resultToEither . parseByteString commandParser mempty

runMainMenuParse :: ByteString -> Either Error Command
runMainMenuParse = resultToEither . parseByteString mainMenuParser mempty

runWordParse :: ByteString -> Either Error Text
runWordParse bs = resultToEither $ parseByteString word mempty bs
    
resultToEither :: Result a -> Either Error a
resultToEither (Failure err') = Left . BadParse $ show err'
resultToEither (Success a) = Right a
