{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser
    ( getMessageContent
    , getMessageEntity
    , getMessageCaption
    , getMessageCaptionEntity
    , getSendingMethod
    , getMessageChatID
    , getLastUpdateNumber
    , getDecodeUpdate
    , PrefixMessage
    , SendingMethod
    , ChatID
    , checkCallbackQuery
    , checkCommand
    , makeRepeatMessage
    ) where

import Config ( defaultHelpMessage ) 
import Users (getUserID, getUsersValue)
import TelegramAPI 
    ( caption_entities
    , caption
    ,  entities
    , CallbackQuery ( _data, _from)
    , User ( id )
    , Animation ( file_id )
    , Update (..)
    , TelegramResponse (result)
    , Contact (phone_number, first_name, last_name, user_id, vcard)
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
import Prelude hiding (id )
import Network.HTTP.Simple (getResponseBody, Response, Query)
import Data.Aeson (eitherDecode, encode)
import Data.Maybe (fromJust, isJust)
import qualified Data.Map as Map
import qualified Data.Text.Encoding as DTE
import qualified Data.Text as T (Text, unpack)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as BC     
import qualified Data.ByteString.Lazy as LBS


type ChatID = BC.ByteString
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

getMessageContent :: Maybe Message -> Query
getMessageContent = maybe [("text", Just "Setting new value")] parseMessageContent
    where parseMessageContent input
           | isJust $ animation input = 
               [( "animation"
                , Just $ DTE.encodeUtf8 $ file_id (fromJust $ animation input :: Animation)
                )]
           | isJust $ audio input = 
               [("audio"
                , Just $ DTE.encodeUtf8 $ file_id (fromJust $ audio input :: Audio)
                )]   
           | isJust $ photo input = 
               [("photo"
                , Just $ DTE.encodeUtf8 $ file_id (head . fromJust $ photo input :: PhotoSize) 
                )]
           | isJust $ document input = 
               [("document"
                , Just $ DTE.encodeUtf8 $ file_id (fromJust $ document input :: Document) 
                )]
           | isJust $ video input = 
               [("video"
                , Just $ DTE.encodeUtf8 $ file_id (fromJust $ video input :: Video)             
                )]
           | isJust $ voice input =
               [("voice"
                , Just $ DTE.encodeUtf8 $ file_id (fromJust $ voice input :: Voice)   
                )]    
           | isJust $ sticker input =
               [("sticker"
                , Just $ DTE.encodeUtf8 $ file_id (fromJust $ sticker input :: Sticker)  
                )]                                                                           
           | isJust $ contact input = 
               [("phone_number"
                , DTE.encodeUtf8 . phone_number <$> contact input)
               ,("first_name" 
                , DTE.encodeUtf8 . first_name <$> contact input)
               ,("last_name"
                , DTE.encodeUtf8 <$> (contact input >>= last_name))
               ,("user_id"
                , BC.pack . show <$> (contact input >>= user_id))
               , ("vcard"
                , DTE.encodeUtf8 <$> (contact input >>= vcard)
                )]
           | isJust $ video_note input = 
               [("video_note"
                , Just $ DTE.encodeUtf8 $ file_id (fromJust $ video_note input :: VideoNote)
                )]
           | isJust $ text input = 
               [("text"
                , Just $ checkCommandMessage $ text input 
                )]
           | otherwise = [("text", Just "Can't parse your message")]

               
checkCommandMessage :: Maybe T.Text -> BC.ByteString
checkCommandMessage maybeText 
    | fromJust maybeText == "/help"           = defaultHelpMessage
    | fromJust maybeText == "/repeat"         = BC.empty
    | fromJust maybeText == "/getMyCommands"  = "[/help, /repeat, /getMyCommands]"
    | otherwise                               = DTE.encodeUtf8 . fromJust $ maybeText   

getMessageEntity :: Maybe Message -> Query
getMessageEntity input = [("entities", fmap (LBS.toStrict . encode . entities) input)]

getMessageCaption :: Maybe Message -> Query
getMessageCaption input = [("caption", fmap DTE.encodeUtf8 (input >>= caption))]

getMessageCaptionEntity :: Maybe Message -> Query
getMessageCaptionEntity input = [("caption_entities", fmap (LBS.toStrict . encode . caption_entities) input)]     
                                              
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

makeRepeatMessage:: TelegramResponse -> Map.Map Int Int -> Query  -- checking value of repeats
makeRepeatMessage newResponse mapUsers = [("text", Just messageForRepeate)]  
    where messageForRepeate = messageFor <> "\n Click on any button to set the value:"
          messageFor = maybe defaultMessage (\number -> 
                            "Number of message repeats: " <> (BC.pack . show) number) maybeNumber
          maybeNumber = getUsersValue (getUserID newResponse) mapUsers                  
          defaultMessage = "Number of message repeats: 1 (default value)\n\
                        \Click on any button to set the value:\n"            
             
