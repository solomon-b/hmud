module HMud.Parser.Token where

import Control.Applicative hiding (many)

import Data.ByteString (ByteString, cons, pack)
import Data.Void
import Data.Text.Encoding
import Data.Text (Text, strip)
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L

type Parser = Text.Megaparsec.Parsec Void ByteString


sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: ByteString -> Parser ByteString
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

signedInteger :: Parser Integer
signedInteger = L.signed sc integer

signedFloat :: Parser Double
signedFloat = L.signed sc float

rword :: ByteString -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [Text]
rws =
    [ "exit"
    , "register"
    , "login"
    , "logout"
    , "help"
    , "whois"
    , "shutdown"
    , "say"
    , "here"
    , "north"
    , "south"
    , "east"
    , "west"
    , "northwest"
    , "northeast"
    , "southwest"
    , "southeast"
    , "up"
    , "down"
    , "n"
    , "s"
    , "e"
    , "w"
    , "ne"
    , "nw"
    , "se"
    , "sw"
    , "u"
    , "d"
    ]

identifier :: Parser Text
identifier = (lexeme . try) (p >>= check)
  where
    p :: Parser Text
    p = strip . decodeUtf8 <$> (cons <$> letterChar <*> (pack <$> many asciiChar))
    check :: Text -> Parser Text
    check str = if str `elem` rws
                then fail $ "keyword " ++ show str ++ " cannot be an identifier"
                 else pure str

phrase :: Parser Text
phrase = (lexeme . try) (p >>= check)
  where
    p :: Parser Text
    p = strip . decodeUtf8 <$> ((pack <$> many asciiChar) <* eof)
    check :: Text -> Parser Text
    check str = if str `elem` rws
                then fail $ "keyword " ++ show str ++ " cannot be an identifier"
                 else pure str
