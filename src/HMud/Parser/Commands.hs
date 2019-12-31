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
import HMud.Types
--import HMud.SqliteLib (User(..))

{-
Main Menu Commands
(l)ogin
(r)egister account
(c)reate player
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

-----------------
--- Main Menu ---
-----------------

pLogin :: Parser Command
pLogin = (rword "login" <|> rword "l") $> Login

pRegister :: Parser Command
pRegister = (rword "register" <|> rword "r") $> Register

pCreate :: Parser Command
pCreate = (rword "create" <|> rword "c") $> Create

pExit :: Parser Command
pExit = (rword "exit" <|> rword "e") $> Exit

pMainMenu :: Parser Command
pMainMenu = pLogin <|> pRegister <|> pCreate <|> pExit

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
  void . optional $ rword "move" <|> rword "m"
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
    room = Here <$ (eof <|> void (symbol "here"))

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

runParse' :: MonadError ParserError m => Parser a -> ByteString -> m a
runParse' parser bs =
    let parsed = runParser parser mempty bs
    in case parsed of
        Right cmd -> return cmd
        Left _   -> throwError BadParse

runParse :: MonadError ParserError m => ByteString -> m Command
runParse = runParse' (try pInGame <|> try pMainMenu <|> parserRaw <|> parserSuppress)

runWordParse :: MonadError ParserError m => ByteString -> m Text
runWordParse = runParse' phrase

resultToEither :: Either a b -> Either ParserError b
resultToEither (Left _) = Left BadParse
resultToEither (Right a) = Right a
