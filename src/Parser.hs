{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser
    ( getMessageContent
    , getMessageEntity
    , getMessageCaption
    , getMessageCaptionEntity
    , getPrefix
    , getSendingMethod
    , getMessageChatID
    , getLastUpdateNumber
    , getDecodeUpdate
    , PrefixMessage
    , SendingMethod
    , ChatID
    , ReseivedMessage
    , checkCallbackQuery
    , checkCommand
    ) where

import Config ( defaultHelpMessage ) 
import Prelude hiding (id )
import qualified Data.ByteString.Char8 as BC 
import TelegramAPI 
    (caption_entities, caption,  entities
    
    , CallbackQuery ( _data, _from)
    , User ( id )
    , Animation ( file_id )
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
import qualified Data.Text as T (Text, unpack)
import Network.HTTP.Simple (getResponseBody, Response, addToRequestQueryString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as LBS


type ChatID = BC.ByteString
type ReseivedMessage = [(BC.ByteString, BC.ByteString)]
type SendingMethod = BC.ByteString
type PrefixMessage = BC.ByteString

getMessageChatID :: Update -> ChatID
getMessageChatID update  
    | isJust $ message update           = BC.pack . show . _id . chat $ fromJust $ message update
    | isJust $ channel_post update     = BC.pack . show . _id . chat $ fromJust $ message update
    | isJust $ callback_query update    = BC.pack . show . id . _from $ fromJust $ callback_query update
    | otherwise                       = error "message don't reseived"

checkCommand :: TelegramResponse -> Bool
checkCommand newResponse = 
    let maybeText = (message . head . result $ newResponse) >>= text
        checkJust txt | txt == "/help"           = True
                      | txt == "/repeat"         = True
                      | txt == "/getMyCommands"  = True
                      | otherwise                = False 
    in maybe False checkJust maybeText

getMessageContent :: Maybe Message -> ReseivedMessage   
getMessageContent = maybe ["text", "Setting new value"] parseMessageContent
    where parseMessageContent input
           | isJust $ animation input = 
               [( "animation"
                , DTE.encodeUtf8 $ file_id ((fromJust $ animation input) :: Animation)
                )]
           | isJust $ audio input = 
               [("audio"
                , DTE.encodeUtf8 $ file_id ((fromJust $ audio input) :: Audio) 
                )]   
           | isJust $ photo input = 
               [("photo"
                , DTE.encodeUtf8 $ file_id ((head . fromJust $ photo input) :: PhotoSize) 
                )]
           | isJust $ document input = 
               [("document"
                , DTE.encodeUtf8 $ file_id (fromJust $ document input :: Document) 
                )]
           | isJust $ video input = 
               [("video"
                , DTE.encodeUtf8 $ file_id (fromJust $ video input :: Video)             
                )]
           | isJust $ voice input =
               [("voice"
                , DTE.encodeUtf8 $ file_id (fromJust $ voice input :: Voice)   
                )]    
           | isJust $ sticker input =
               [("sticker"
                , DTE.encodeUtf8 $ file_id (fromJust $ sticker input :: Sticker)  
                )]                                                                           
           | isJust $ contact input = 
               [("phone_number"
                , makeContactMessage $ contact input
           | isJust $ video_note input = 
               DTE.encodeUtf8 $ file_id (fromJust $ video_note input :: VideoNote)
           | isJust $ text input  = checkCommandMessage $ text input 
           | otherwise = "Can't parse your message"

makeContactMessage 


    
               
checkCommandMessage :: Maybe T.Text -> BC.ByteString
checkCommandMessage maybeText 
    | fromJust maybeText == "/help"           = defaultHelpMessage
    | fromJust maybeText == "/repeat"         = BC.empty
    | fromJust maybeText == "/getMyCommands"  = "[/help, /repeat]"
    | otherwise                               = DTE.encodeUtf8 . fromJust $ maybeText   

getMessageEntity :: Maybe Message -> Maybe BC.ByteString
getMessageEntity = fmap (LBS.toStrict . encode . entities)

getMessageCaption :: Maybe Message -> Maybe BC.ByteString
getMessageCaption input = fmap DTE.encodeUtf8 (input >>= caption)

getMessageCaptionEntity :: Maybe Message -> Maybe BC.ByteString
getMessageCaptionEntity = fmap (LBS.toStrict . encode . caption_entities)      
                                              
getSendingMethod :: Maybe Message -> SendingMethod
getSendingMethod = maybe "/sendMessage" parseMessageContent 
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
           | otherwise               = "/sendMessage"
         

getPrefix :: Maybe Message -> PrefixMessage
getPrefix = maybe "text" parseMessageContent 
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
           | otherwise               = "text"
         

checkCallbackQuery :: TelegramResponse -> Maybe Int
checkCallbackQuery newResponse = fmap (read . T.unpack) (maybeCallback >>= _data )
    where maybeCallback = callback_query . head . result $ newResponse

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
             
