{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser
    ( getMessageContent
    , getPrefix
    , getSendingMethod
    , getMessageChatID
    , getLastUpdateNumber
    , getDecodeUpdate
    , PrefixMessage
    , SendingMethod
    , ChatID
    , ReseivedMessage
    ) where
import Config ( defaultHelpMessage, defaultRepeateMessage ) 
import Prelude hiding (id )
import qualified Data.ByteString.Char8 as BC 
import TelegramAPI 
    ( Animation ( file_id )
    , Update (..)
    , TelegramResponse (result)
    , Contact (phone_number, first_name)
    , Sticker (file_id)
    , Voice(file_id)
    , Video ( file_id )
    , Document ( file_id )
    , Audio ( file_id )
    , PhotoSize ( file_id )
    , Chat (_id)
    , Message (chat, text, sticker, photo, voice, contact, animation,
              audio, video, video_note, document)
    , VideoNote (..)
    )
import Data.Maybe (fromJust, isJust)
import qualified Data.Text.Encoding as DTE
import qualified Data.Text as T (Text)
import Network.HTTP.Simple (getResponseBody, Response)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Aeson (eitherDecode)


type ChatID = BC.ByteString
type ReseivedMessage = BC.ByteString
type SendingMethod = BC.ByteString
type PrefixMessage = BC.ByteString

getMessageChatID :: Maybe Message -> ChatID
getMessageChatID maybeMessage = case maybeMessage of
    Just message -> BC.pack . show . _id . chat $ message
    Nothing      -> error "message don't reseived"

getMessageContent :: Maybe Message -> ReseivedMessage   
getMessageContent maybeMessage = case maybeMessage of 
    Just message -> parseMessageContent message
    Nothing      -> error "message don't reseived"  
    where parseMessageContent input
           | isJust $ animation input  = 
               DTE.encodeUtf8 $ file_id ((fromJust $ animation input) :: Animation)
           | isJust $ audio input  = 
               DTE.encodeUtf8 $ file_id ((fromJust $ audio input) :: Audio)    
           | isJust $ photo input  = 
               DTE.encodeUtf8 $ file_id ((head . fromJust $ photo input) :: PhotoSize) 
           | isJust $ document input  = 
               DTE.encodeUtf8 $ file_id (fromJust $ document input :: Document) 
           | isJust $ video input  = 
               DTE.encodeUtf8 $ file_id (fromJust $ video input :: Video)             
           | isJust $ voice input  = 
               DTE.encodeUtf8 $ file_id (fromJust $ voice input :: Voice)       
           | isJust $ sticker input  = 
               DTE.encodeUtf8 $ file_id (fromJust $ sticker input :: Sticker)                                                                             
           | isJust $ contact input  = 
               DTE.encodeUtf8 $ (phone_number . fromJust $ contact input) 
               <> "&first_name=" <> (first_name . fromJust $ contact input)
           | isJust $ video_note input = 
               DTE.encodeUtf8 $ file_id (fromJust $ video_note input :: VideoNote)
           | isJust $ text input  = checkCommandMessage $ text input 
           | otherwise = "Can't parse your message"

checkCommandMessage :: Maybe T.Text -> ReseivedMessage
checkCommandMessage maybeText 
    | fromJust maybeText == "/help"     = defaultHelpMessage
    | fromJust maybeText == "/repeat"   = defaultRepeateMessage
    | otherwise                         = DTE.encodeUtf8 . fromJust $ maybeText     

getSendingMethod :: Maybe Message -> SendingMethod
getSendingMethod maybeMessage = case maybeMessage of 
    Just message -> parseMessageContent message
    Nothing      -> error "message don't reseived"  
    where parseMessageContent input
           | isJust $ animation input  = "/sendAnimation"
           | isJust $ audio input  = "/sendAudio"    
           | isJust $ photo input  = "/sendPhoto" 
           | isJust $ document input  = "/sendDocument" 
           | isJust $ video input  = "/sendVideo"   
           | isJust $ video_note input = "/sendVideoNote"          
           | isJust $ voice input  = "/sendVoice"        
           | isJust $ sticker input  = "/sendSticker"             
           | isJust $ contact input  = "/sendContact" 
           | otherwise = "/sendMessage"

getPrefix :: Maybe Message -> PrefixMessage
getPrefix maybeMessage = case maybeMessage of 
    Just message -> parseMessageContent message
    Nothing      -> error "message don't reseived"  
    where parseMessageContent input
           | isJust $ sticker input = "sticker"
           | isJust $ photo input = "photo"
           | isJust $ voice input = "voice"
           | isJust $ contact input = "phone_number"
           | isJust $ animation input = "animation"
           | isJust $ audio input = "audio"
           | isJust $ video  input = "video"
           | isJust $ video_note input = "video_note"
           | isJust $ document input = "document"
           | otherwise  = "text"

getDecodeUpdate :: Response L.ByteString -> TelegramResponse
getDecodeUpdate reseivingBC = let jsonBody = getResponseBody reseivingBC
                                  telegramResponse = eitherDecode jsonBody
                              in case telegramResponse of 
                                    Left noDec -> error $ "can't decode Update: " <> noDec
                                    Right res  -> res 

getLastUpdateNumber :: TelegramResponse -> Int
getLastUpdateNumber decodeUpdate =  
            if null (result decodeUpdate) 
            then 0
            else update_id . head . result $ decodeUpdate 
             
