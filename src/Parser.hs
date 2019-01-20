module Parser (commandParser, resultToEither, runParse, runWordParse, word)  where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Functor (($>))
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Trifecta

import Types (User(..), Command(..), Direction(..))


word :: Parser Text
word = token $ T.pack <$> some letter

parserGetUsers :: Parser Command
parserGetUsers = symbol "getUsers" $> GetUsers

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
    return $ AddUser (User 0 username' password' 1)

parserExit :: Parser Command
parserExit = token $ do
    void $ symbol "exit"
    return Exit

parserShutdown :: Parser Command
parserShutdown = token $ do
    void $ symbol "shutdown"
    return Shutdown

parserRegister :: Parser Command
parserRegister = token $ do
    void $ symbol "register"
    return Register

parserLogin :: Parser Command
parserLogin = token $ do
    void $ symbol "login"
    return Login

parserLogout :: Parser Command
parserLogout = token $ do
    void $ symbol "logout"
    return Logout

parserWhois :: Parser Command
parserWhois = token $ do
    void $ symbol "whois"
    return Whois

parserSay :: Parser Command
parserSay = token $ do
    void $ symbol "say"
    str <- anyChar `manyTill` (char '\r' <|> char '\n')
    return $ Say (T.pack str)

parserLook :: Parser Command
parserLook = token $ do
    void $ symbol "look"
    return Look

parserEcho :: Parser Command
parserEcho = token $ do
    void $ symbol "echo"
    str <- anyChar `manyTill` (char '\r' <|> char '\n')
    return $ Echo (T.pack str)

symbolMove :: Parser (Maybe String)
symbolMove = optional $ symbol "move" <|> symbol "mv"

parserNorth :: Parser Command
parserNorth = token $ do
    void symbolMove
    void $ symbol "n" <|> symbol "north" <|> symbol "N"
    return $ Move N

parserSouth :: Parser Command
parserSouth = token $ do
    void symbolMove
    void $ symbol "s" <|> symbol "south" <|> symbol "S"
    return $ Move S

parserEast :: Parser Command
parserEast = token $ do
    void symbolMove
    void $ symbol "e" <|> symbol "east" <|> symbol "E"
    return $ Move E

parserWest :: Parser Command
parserWest = token $ do
    void symbolMove
    void $ symbol "w" <|> symbol "west" <|> symbol "W"
    return $ Move W

parserNorthWest :: Parser Command
parserNorthWest = token $ do
    void symbolMove
    void $ symbol "nw" <|> symbol "northwest" <|> symbol "NW"
    return $ Move NE

parserNorthEast :: Parser Command
parserNorthEast = token $ do
    void symbolMove
    void $ symbol "ne" <|> symbol "northeast" <|> symbol "ne"
    return $ Move NE

parserSouthEast :: Parser Command
parserSouthEast = token $ do
    void symbolMove
    void $ symbol "se" <|> symbol "southeast" <|> symbol "SE"
    return $ Move SE

parserSouthWest :: Parser Command
parserSouthWest = token $ do
    void symbolMove
    void $ symbol "sw" <|> symbol "southwest" <|> symbol "SW"
    return $ Move SW

parserMove :: Parser Command
parserMove =  parserNorth
          <|> parserSouth
          <|> parserEast
          <|> parserWest
          <|> parserNorthWest
          <|> parserNorthEast
          <|> parserSouthWest
          <|> parserSouthEast

commandParser :: Parser Command
commandParser =  parserGetUsers 
             <|> parserGetUser 
             <|> parserAddUser 
             <|> parserExit
             <|> parserShutdown
             <|> parserEcho
             <|> parserRegister
             <|> parserLook
             <|> parserLogin
             <|> parserLogout
             <|> parserWhois
             <|> parserSay
             <|> parserMove


runParse :: ByteString -> Either Text Command
runParse = resultToEither . parseByteString commandParser mempty

runWordParse :: ByteString -> Either Text Text
runWordParse bs = resultToEither $ parseByteString word mempty bs
    
resultToEither :: Result a -> Either Text a
resultToEither (Failure err') = Left . T.pack $ show err'
resultToEither (Success a) = Right a
