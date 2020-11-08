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

import Prelude hiding (id)
import qualified Data.ByteString.Char8 as BC 
import TelegramAPI 
    ( Update (message, update_id)
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
    , Message (chat, text, sticker, photo, from, voice, contact, animation,
              audio, video, video_note, document)
    , VideoNote (file_idVN)
    , User (id)
    )
import Data.Maybe (fromJust, isJust)
--import qualified Data.Text.Encoding as DTE
import Data.Text (unpack)
import Network.HTTP.Simple (getResponseBody, Response)
import qualified Data.ByteString.Lazy as L
import Data.Aeson (eitherDecode)
import qualified Data.Map as Map 


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
           | isJust $ text input  = 
               unpack . fromJust $ text input 
           | isJust $ animation input  = 
               unpack . file_idAn . fromJust $ animation input
           | isJust $ audio input  = 
               unpack . file_idAud . fromJust $ audio input    
           | isJust $ photo input  = 
               unpack .file_id . head . fromJust $ photo input 
           | isJust $ document input  = 
               unpack . file_idDoc . fromJust $ document input 
           | isJust $ video input  = 
               unpack . file_idVid . fromJust $ video input             
           | isJust $ voice input  = 
               unpack . file_idV . fromJust $ voice input         
           | isJust $ sticker input  = 
               unpack . file_idSt . fromJust $ sticker input                                                                               
           | isJust $ contact input  = 
               unpack $ (phone_number . fromJust $ contact input) 
               <> "&first_name=" <> (first_nameCon . fromJust $ contact input)
           | isJust $ video_note input = 
               unpack . file_idVN . fromJust $ video_note input
     --      | isJust $ entities input = unpack . fromJust $ entities input 
           | otherwise = "Can't return your message yet!"
                                   
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

getDecodeUpdate :: Response L.ByteString -> IO TelegramResponse
getDecodeUpdate reseivingBC = do
    let jsonBody = getResponseBody reseivingBC
    let telegramResponse = eitherDecode jsonBody
    case telegramResponse of 
        Left noDec -> return . error $ "can't decode Update: " <> noDec
        Right res  -> return res 

getLastUpdateNumber :: TelegramResponse -> Int
getLastUpdateNumber decodeUpdate =  
            if null (result decodeUpdate) 
            then 0
            else update_id . head . result $ decodeUpdate 
             
