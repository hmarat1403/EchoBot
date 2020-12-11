{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser
    ( getMessageContent
    , getSendingMethod
    , getMessageChatID
    , getMessageID
    , getLastUpdateNumber
    , getDecodeUpdate
    , PrefixMessage
    , SendingMethod
    , ChatID
    , MessageID
    , checkCallbackQuery
    , checkCommand
    , makeRepeatMessage
    , makeCopyMessage
    , getMessageEntity
    , getMessageCaptionEntity
    ) where

import Config ( defaultHelpMessage ) 
import Users (getUserID, getUsersValue)
import TelegramAPI 
    ( entities
    , caption_entities
    , CallbackQuery ( _data, _from)
    , User ( id )
    , Update (..)
    , TelegramResponse (result)
    , Chat (_id)
    , Message (message_id, chat, text)
    )
import Prelude hiding (id )
import Network.HTTP.Simple (getResponseBody, Response, Query)
import Data.Aeson (encode, eitherDecode)
import Data.Maybe (fromJust, isJust)
import Control.Applicative ( Alternative((<|>)) )
import qualified Data.Map as Map
import qualified Data.Text as T (unpack)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as BC     
import qualified Data.ByteString.Lazy as LBC



type ChatID = BC.ByteString
type MessageID = BC.ByteString
type SendingMethod = BC.ByteString
type PrefixMessage = BC.ByteString

checkCommand :: TelegramResponse -> Bool
checkCommand newResponse = 
    let maybeText = (message . head . result $ newResponse) >>= text
        checkJust txt | txt == "/help"           = True
                      | txt == "/repeat"         = True
                      | txt == "/getMyCommands"  = True
                      | otherwise                = False 
    in maybe False checkJust maybeText

               
checkCommandMessage :: Update -> Query
checkCommandMessage update 
        | fromJust maybeText == "/help"           = [("text", Just defaultHelpMessage)]
        | fromJust maybeText == "/repeat"         = [("text", Just BC.empty)]
        | fromJust maybeText == "/getMyCommands"  = [("text", Just "[/help, /repeat, /getMyCommands]")]
        | otherwise                               = makeCopyMessage chat mess 
        where maybeText = message update >>= text
              chat = getMessageChatID update    
              mess = getMessageID update     

                                             
getSendingMethod :: Update -> SendingMethod
getSendingMethod update = maybe "/SendMessage" parseMessageContent maybeMessage
    where maybeMessage = message update <|> channel_post update
          parseMessageContent input 
            | text input == Just "/help"           = "/SendMessage" 
            | text input == Just "/repeat"         = "/SendMessage"
            | text input == Just "/getMyCommands"  = "/SendMessage"
            | otherwise                            = "/CopyMessage"
                          
        
checkCallbackQuery :: TelegramResponse -> Maybe Int          
checkCallbackQuery newResponse = fmap (read . T.unpack) (maybeCallback >>= _data )
    where maybeCallback = callback_query . head . result $ newResponse

getDecodeUpdate :: Response L.ByteString -> TelegramResponse    -- parse response from JSON
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

makeRepeatMessage:: TelegramResponse -> Map.Map Int Int -> Query  -- checking value of repeats and get message for sending
makeRepeatMessage newResponse mapUsers = [("text", Just messageForRepeate)]  
    where messageForRepeate = messageFor <> "\n Click on any button to set the value:"
          messageFor = maybe defaultMessage (\number -> 
                            "Number of message repeats: " <> (BC.pack . show) number) maybeNumber
          maybeNumber = getUsersValue (getUserID newResponse) mapUsers                  
          defaultMessage = "Number of message repeats: 1 (default value)\n\
                        \Click on any button to set the value:\n"            
             
makeCopyMessage :: ChatID -> MessageID -> Query
makeCopyMessage chatID messageID = 
    [ ("from_chat_id", Just chatID)
    , ("message_id", Just messageID)
    ]

getMessageContent :: Update -> Query  -- get query string fo answer
getMessageContent update 
    | isJust $ callback_query update  = [("text", Just "Setting new value")]
    | otherwise                       = parseMessageContent maybeMessage
    where parseMessageContent input = if isJust $ input >>= text
                                      then checkCommandMessage update
                                      else makeCopyMessage chat mess 
          maybeMessage = message update <|> channel_post update 
          chat = getMessageChatID update    
          mess = getMessageID update                                   

getMessageChatID :: Update -> ChatID
getMessageChatID update  
    | isJust $ message update         = BC.pack . show . _id . chat $ fromJust $ message update
    | isJust $ channel_post update    = BC.pack . show . _id . chat $ fromJust $ message update
    | isJust $ callback_query update  = BC.pack . show . id . _from $ fromJust $ callback_query update
    | otherwise                       = error "message don't reseived"

getMessageID :: Update -> MessageID
getMessageID update 
    | isJust $ message update          = BC.pack . show . message_id $ fromJust $ message update
    | isJust $ channel_post update     = BC.pack . show . message_id $ fromJust $ message update
    | isJust $ callback_query update   = BC.pack . show . id . _from $ fromJust $ callback_query update
    | otherwise                        = error "message don't reseived"              

getMessageEntity :: Maybe Message -> Query
getMessageEntity input = [("entities", fmap (LBC.toStrict . encode . entities) input)]


getMessageCaptionEntity :: Maybe Message -> Query
getMessageCaptionEntity input = [("caption_entities", fmap (LBC.toStrict . encode . caption_entities) input)]      