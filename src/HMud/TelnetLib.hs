module HMud.TelnetLib (processStream, Buffer(..)) where

import Control.Monad.State
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

data TelnetMessage =
      ASCII Word8      --   032 >= word8 <= 127
    | Word  Word8      --   031 >= word8 >= 128
    | SOH              --   003
    | SE               --   240
    | NOP              --   241
    | DataMark         --   242
    | Break            --   243
    | InterruptProcess --   244
    | AbortOutput      --   245
    | AreYouThere      --   246
    | EraseCharacter   --   247
    | EraseLine        --   248
    | GoAhead          --   249
    | SB               --   250
    | Will             --   251
    | Wont             --   252
    | Do               --   253
    | Dont             --   254
    | IAC              --   255
    deriving (Show, Eq)

-- data TelnetCommand =
--       WillEcho
--     | WontEcho
--     | DoEcho
--     | DontEcho

newtype Buffer = Buffer { unBuffer :: Maybe ByteString } deriving (Semigroup, Monoid, Show)

data MessageMode = Normal | Command | SubNegotiation
data MessageState = MessageState
    { getBuffer      :: Buffer
    , getMessageMode :: MessageMode
    }

toTelnetWord :: Word8 -> TelnetMessage
toTelnetWord w
    | w == 1               = SOH -- (ECHO)
    | w >= 32 && w <= 127  = ASCII w -- . T.singleton $ chr (fromIntegral w)
    | w == 240             = SE
    | w == 241             = NOP
    | w == 242             = DataMark
    | w == 243             = Break
    | w == 244             = InterruptProcess
    | w == 245             = AbortOutput
    | w == 246             = AreYouThere
    | w == 247             = EraseCharacter
    | w == 248             = EraseLine
    | w == 249             = GoAhead
    | w == 250             = SB
    | w == 251             = Will
    | w == 252             = Wont
    | w == 253             = Do
    | w == 254             = Dont
    | w == 255             = IAC
    | otherwise            = Word w

charToBuffer :: Word8 -> Buffer
charToBuffer w 
    | w <= 255 = Buffer . Just . BS.singleton $ fromIntegral w
    | otherwise = mempty

addWordToBuffer :: Word8 -> Buffer -> Buffer
addWordToBuffer w (Buffer Nothing) = charToBuffer w
addWordToBuffer w bs = Buffer $ (flip BS.snoc w) <$> unBuffer bs

eraseCharFromBuffer :: Buffer -> Buffer
eraseCharFromBuffer (Buffer (Just text)) =
    case BS.unsnoc text of
        Just (buffer', _) 
            | buffer' == BS.empty -> Buffer Nothing
            | otherwise -> Buffer $ Just buffer'
        Nothing         -> mempty
eraseCharFromBuffer _ = mempty

handleStream :: Word8 -> State MessageState ()
handleStream word = do
    buffer <- gets getBuffer
    mode   <- gets getMessageMode
    case mode of
        Normal ->
            case toTelnetWord word of
                -- TODO: Handle '\r\n' in mid stream and '\x08' for clients
                -- that stream characters as typed.
                ASCII c   -> put $ MessageState (addWordToBuffer c buffer) Normal
                IAC       -> put $ MessageState buffer Command
                _         -> put $ MessageState buffer mode
        Command ->
            -- TODO: Perform actual behavior for remaining TelnetCommands.
            -- TODO: Correctly Handle SOH Acknowledgment
            case toTelnetWord word of
                EraseCharacter  -> put $ MessageState (eraseCharFromBuffer buffer) Normal
                EraseLine       -> put $ MessageState mempty Normal
                SB              -> put $ MessageState buffer  SubNegotiation
                Will            -> put $ MessageState buffer  Command
                Wont            -> put $ MessageState buffer  Command
                Do              -> put $ MessageState buffer  Command
                Dont            -> put $ MessageState buffer  Command
                SOH             -> put $ MessageState buffer  Normal
                _               -> put $ MessageState buffer  Normal
        SubNegotiation ->
            case toTelnetWord word of
                SE -> put $ MessageState buffer Normal
                _  -> put $ MessageState buffer SubNegotiation

processStream :: ByteString -> Buffer
processStream bs =
    let stream = BS.unpack bs
        startingState = MessageState mempty Normal
        (MessageState buff _)  = execState (mapM_ handleStream stream) startingState
    in buff

{-

A Normal Message:
BS= [65, 66, 67] -> return . Just $ T.pack "abc"

Client sending an acknowldgement:
BS = [255,253,3]  -> return Nothing

Normal Message with command at end:
BS = [65, 66, 67, 255. 248] -> return . Just $ T.pack "abc"

Cmd then Message:
BS = [255, 248, 3, 65, 66, 67] -> return . Just $ T.pack "abc"

-}
