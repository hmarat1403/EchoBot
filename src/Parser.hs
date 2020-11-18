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
    ( Update ( update_id)
    , TelegramResponse (result)
    , Contact (phone_number, first_nameCon)
    , Sticker (file_idSt)
    , Voice(file_idV)
    , Video (file_idVid)
    , Document (file_idDoc)
    , Audio (file_idAud)
    , Animation (file_idAn)
    , PhotoSize (file_id)
    , Chat (chat_id)
    , Message (chat, text, sticker, photo, voice, contact, animation,
              audio, video, video_note, document)
    , VideoNote (file_idVN)
    )
import Data.Maybe (fromJust, isJust)
--import qualified Data.Text.Encoding as DTE
import qualified Data.Text as T (Text, unpack, head, tail, cons)
import Network.HTTP.Simple (getResponseBody, Response)
import qualified Data.ByteString.Lazy as L
import Data.Aeson (eitherDecode)


type ChatID = BC.ByteString
type ReseivedMessage = String
type SendingMethod = BC.ByteString
type PrefixMessage = BC.ByteString

getMessageChatID :: Maybe Message -> ChatID
getMessageChatID maybeMessage = case maybeMessage of
    Just message -> BC.pack . show . chat_id . chat $ message
    Nothing      -> error "message don't reseived"

getMessageContent :: Maybe Message -> ReseivedMessage   
getMessageContent maybeMessage = case maybeMessage of 
    Just message -> parseMessageContent message
    Nothing      -> error "message don't reseived"  
    where parseMessageContent input
           | isJust $ animation input  = 
               T.unpack . file_idAn . fromJust $ animation input
           | isJust $ audio input  = 
               T.unpack . file_idAud . fromJust $ audio input    
           | isJust $ photo input  = 
               T.unpack .file_id . head . fromJust $ photo input 
           | isJust $ document input  = 
               T.unpack . file_idDoc . fromJust $ document input 
           | isJust $ video input  = 
               T.unpack . file_idVid . fromJust $ video input             
           | isJust $ voice input  = 
               T.unpack . file_idV . fromJust $ voice input         
           | isJust $ sticker input  = 
               T.unpack . file_idSt . fromJust $ sticker input                                                                               
           | isJust $ contact input  = 
               T.unpack $ (phone_number . fromJust $ contact input) 
               <> "&first_name=" <> (first_nameCon . fromJust $ contact input)
           | isJust $ video_note input = 
               T.unpack . file_idVN . fromJust $ video_note input
           | isJust $ text input  = checkCommandMessage $ text input 
           | otherwise = "Can't parse your message"

checkCommandMessage :: Maybe T.Text -> ReseivedMessage
checkCommandMessage maybeText 
    | fromJust maybeText == "/help"      = BC.unpack defaultHelpMessage
    | fromJust maybeText == "/repeat"   = BC.unpack defaultRepeateMessage
    | (T.head . fromJust $ maybeText) == '#' = T.unpack . T.tail . fromJust $ maybeText
    | otherwise                          = T.unpack . fromJust $ maybeText     

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
           | isJust $ sticker input = "&sticker="
           | isJust $ photo input = "&photo="
           | isJust $ voice input = "&voice="
           | isJust $ contact input = "&phone_number="
           | isJust $ animation input = "&animation="
           | isJust $ audio input = "&audio="
           | isJust $ video  input = "&video="
           | isJust $ video_note input = "&video_note="
           | isJust $ document input = "&document="
           | otherwise  = "&text="

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
             
