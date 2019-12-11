module HMud.Parser.Commands where

import Control.Applicative
import Control.Monad.Except

import Data.ByteString (ByteString)
import Data.Functor
import Data.Text hiding (foldl1, pack)

import Text.Megaparsec
import Text.Megaparsec.Byte

import HMud.Errors
import HMud.Parser.Token
import HMud.SqliteLib (User(..))

{-
Main Menu Commands
(l)ogin
(r)egister
(e)exit

In Game User Commands
(l)ook [direction | object | here | ""]
(s)ay <text>
logout
(h)elp
emote <text>
(m)ove [dir]
(n)orth
(s)outh
(e)ast
(w)est
(u)p
(d)own
northwest
northeast
southwest
southeast
ne
nw
se
sw
-}

data Target = Object Text | Dir Direction | Room
  deriving (Eq, Show)

data Command
    = GetUsers
    | GetUser Text
    | AddUser User
    | Echo Text
    | Shutdown
    | Register
    | Raw Text
    | Look Target
    | Login
    | Logout
    | Exit
    | Help
    | Whois
    | Say Text
    | Emote Text
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


-----------------
--- Main Menu ---
-----------------

pLogin :: Parser Command
pLogin = (rword "login" <|> rword "l") $> Login

pRegister :: Parser Command
pRegister = (rword "register" <|> rword "r") $> Register

pExit :: Parser Command
pExit = (rword "exit" <|> rword "e") $> Exit

pMainMenu :: Parser Command
pMainMenu = pLogin <|> pRegister <|> pExit

------------------
--- Directions ---
------------------

pNorth :: Parser Direction
pNorth = (rword "north" <|> rword "n") $> N

pSouth :: Parser Direction
pSouth = (rword "south" <|> rword "s") $> S

pEast :: Parser Direction
pEast = (rword "east" <|> rword "e") $> E

pWest :: Parser Direction
pWest = (rword "west" <|> rword "w") $> W

pNorthEast :: Parser Direction
pNorthEast = (rword "northeast" <|> rword "ne") $> NE

pNorthWest :: Parser Direction
pNorthWest = (rword "northwest" <|> rword "nw") $> NW

pSouthEast :: Parser Direction
pSouthEast = (rword "southeast" <|> rword "se") $> SE

pSouthWest :: Parser Direction
pSouthWest = (rword "southwest" <|> rword "sw") $> SW

pUp :: Parser Direction
pUp = (rword "up" <|> rword "u") $> U

pDown :: Parser Direction
pDown = (rword "down" <|> rword "d") $> D

pDirection :: Parser Direction
pDirection = choice [pNorth , pSouth , pEast , pWest , pNorthEast , pNorthWest , pSouthEast , pSouthWest]

---------------
--- In Game ---
---------------

pSay :: Parser Command
pSay = do
  rword "say"
  sc
  str <- phrase
  pure $ Say str

pLogout :: Parser Command
pLogout = (rword "logout" <|> rword "l") $> Logout

pHelp :: Parser Command
pHelp = (rword "help" <|> rword "h") $> Help

pMove :: Parser Command
pMove = do
  optional $ rword "move" <|> rword "m"
  Move <$> pDirection

pLook :: Parser Command
pLook = do
  rword "look" <|> rword "l"
  target <- room <|> direct <|> object
  pure $ Look target
  where
    object :: Parser Target
    object = Object <$> identifier
    direct :: Parser Target
    direct = Dir <$> pDirection
    room :: Parser Target
    room = Room <$ (eof <|> void (symbol "here"))

pInGame :: Parser Command
pInGame = pSay <|> pLogout <|> pHelp <|> pLook <|> pMove

--------------
--- Telnet ---
--------------

--sendHandle' sock $ BS.pack [255,251,1]
--("\255\\\251\\\SOH", Nothing   , SuppressEcho)
parserSuppress :: Parser Command
parserSuppress = do
    void $ char 3
    eof
    pure SuppressEcho

parserRaw :: Parser Command
parserRaw = Raw <$> phrase

-----------------------
--- Parse Execution ---
-----------------------

runParse' :: MonadError AppError m => Parser a -> ByteString -> m a
runParse' parser bs =
    let parsed = runParser parser mempty bs
    in case parsed of
        Right cmd -> return cmd
        Left _   -> throwError InvalidCommand

runParse :: MonadError AppError m => ByteString -> m Command
runParse = runParse' (try pInGame <|> try pMainMenu <|> parserRaw <|> parserSuppress)

runWordParse :: MonadError AppError m => ByteString -> m Text
runWordParse = runParse' phrase

resultToEither :: Either a b -> Either AppError b
resultToEither (Left _) = Left InvalidCommand
resultToEither (Right a) = Right a
