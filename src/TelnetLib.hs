{-# LANGUAGE OverloadedStrings #-}
module TelnetLib where

--import Control.Concurrent
import Control.Monad.State
import Data.Text()
import Data.Text.Encoding (encodeUtf8)
import Data.Word
import Network.Socket hiding (recv)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket.ByteString (recv, sendAll)


data TelnetCommand =
      Message Word8     -- <128
    | SE               --  240
    | NOP              --  241
    | DataMark         --  242
    | Break            --  243
    | InterruptProcess --  244
    | AbortOutput      --  245
    | AreYouThere      --  246
    | EraseCharacter   --  247
    | EraseLine        --  248
    | GoAhead          --  249
    | SB               --  250
    | Will             --  251
    | Wont             --  252
    | Do               --  253
    | Dont             --  254
    | IAC              --  255
    deriving (Show, Eq)

data MessageMode = Normal | Command | SubNegotiation
data MessageState = MessageState { getBuffer :: Maybe ByteString, getMessageMode :: MessageMode }

toTelnetCommand :: Word8 -> TelnetCommand
toTelnetCommand w
    | w == 240  = SE
    | w == 241  = NOP
    | w == 242  = DataMark
    | w == 243  = Break
    | w == 244  = InterruptProcess
    | w == 245  = AbortOutput
    | w == 246  = AreYouThere
    | w == 247  = EraseCharacter
    | w == 248  = EraseLine
    | w == 249  = GoAhead
    | w == 250  = SB
    | w == 251  = Will
    | w == 252  = Wont
    | w == 253  = Do
    | w == 254  = Dont
    | w == 255  = IAC
    | otherwise = Message w


addWordToBuffer :: Word8 -> Maybe ByteString -> Maybe ByteString
addWordToBuffer w Nothing = Just $ BS.pack [w]
addWordToBuffer w (Just buffer) = Just $ BS.append buffer (BS.pack [w])

eraseCharFromBuffer :: Maybe ByteString -> Maybe ByteString
eraseCharFromBuffer Nothing = Nothing
eraseCharFromBuffer (Just buffer) =
    case BS.unsnoc buffer of
        Just (buffer', _) -> Just buffer'
        Nothing -> Nothing

handleStream :: Word8 -> State MessageState ()
handleStream word = do
    buffer <- gets getBuffer
    mode <- gets getMessageMode
    case mode of
            Normal ->
                case toTelnetCommand word of
                    -- TODO: Handle '\r\n' in mid stream and '\x08' for clients
                    -- that stream characters as typed.
                    Message w -> put $ MessageState (addWordToBuffer w buffer) Normal
                    IAC -> put $ MessageState buffer Command
                    _ -> put $ MessageState buffer mode
            Command -> 
                -- TODO: Perform actual behavior for remaining TelnetCommands. 
                case toTelnetCommand word of
                    EraseCharacter -> put $ MessageState (eraseCharFromBuffer buffer) Normal
                    EraseLine -> put $ MessageState Nothing Normal
                    SB -> put $ MessageState buffer SubNegotiation
                    Will -> put $ MessageState buffer Command
                    Wont -> put $ MessageState buffer Command
                    Do -> put $ MessageState buffer Command
                    Dont -> put $ MessageState buffer Command
                    _ -> put $ MessageState buffer Normal
            SubNegotiation ->
                case toTelnetCommand word of
                    SE -> put $ MessageState buffer Normal
                    _ -> put $ MessageState buffer SubNegotiation

processStream :: ByteString -> MessageState
processStream bs = 
    let stream = BS.unpack bs
        startingState = MessageState Nothing Normal
    in execState (mapM_ handleStream stream) startingState

prompt :: Socket -> ByteString -> IO ByteString
prompt sock prefix = do
    sendAll sock (BS.append prefix (BS.pack [255, 249]))
    rawMsg <- recv sock 1024
    print $ BS.append (encodeUtf8 "raw message: ") rawMsg
    let (MessageState msg _) = processStream rawMsg
    case msg of
        Nothing -> prompt sock ""
        Just msg' -> do
            print $ BS.append (encodeUtf8 "message: ") msg'
            return msg'
