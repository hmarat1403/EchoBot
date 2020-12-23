{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser
    ( getMessageContent
    , getSendingMethod
    , getChatID
    , getMessageID
    , getLastUpdateNumber
    , getDecodeUpdate
    , PrefixMessage
    , SendingMethod
    , ChatID
    , MessageID
    , checkCallbackQuery
    , checkCommand
    , checkNullUpdate
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


getDecodeUpdate :: Response L.ByteString -> TelegramResponse    -- parse response from JSON
getDecodeUpdate reseivingBC = let jsonBody = getResponseBody reseivingBC
                                  telegramResponse = eitherDecode jsonBody
                              in case telegramResponse of 
                                    Left noDec -> error $ "can't decode last reseiving update: " <> noDec
                                    Right res  -> res 

checkNullUpdate :: TelegramResponse -> Bool
checkNullUpdate = null . result

getLastUpdateNumber :: TelegramResponse -> Int  
getLastUpdateNumber newResponse = if checkNullUpdate newResponse
                                  then 0
                                  else update_id . head . result $ newResponse                                    

checkCommand :: TelegramResponse -> Bool
checkCommand newResponse = 
    let maybeText = (message . head . result $ newResponse) >>= text
        checkJust txt | txt == "/help"           = True
                      | txt == "/repeat"         = True
                      | txt == "/getMyCommands"  = True
                      | otherwise                = False 
    in maybe False checkJust maybeText

               
checkCommandMessage :: TelegramResponse -> Query
checkCommandMessage newResponse 
        | fromJust maybeText == "/help"           = [("text", Just defaultHelpMessage)]
        | fromJust maybeText == "/repeat"         = [("text", Just BC.empty)]
        | fromJust maybeText == "/getMyCommands"  = [("text", Just "[/help, /repeat, /getMyCommands]")]
        | otherwise                               = makeCopyMessage newResponse 
        where maybeText = ((message . head . result $ newResponse) >>= text) 
                      <|> ((channel_post . head . result $ newResponse) >>= text)
              

                                             
getSendingMethod :: TelegramResponse -> SendingMethod
getSendingMethod newResponse = maybe "/sendMessage" parseMessageContent maybeMessage
    where maybeMessage = message update <|> channel_post update
          update = head . result $ newResponse
          parseMessageContent input 
            | text input == Just "/help"           = "/sendMessage" 
            | text input == Just "/repeat"         = "/sendMessage"
            | text input == Just "/getMyCommands"  = "/sendMessage"
            | otherwise                            = "/copyMessage"
                          
        
checkCallbackQuery :: TelegramResponse -> Maybe Int          
checkCallbackQuery newResponse = fmap (read . T.unpack) (maybeCallback >>= _data )
    where maybeCallback = callback_query . head . result $ newResponse


makeRepeatMessage:: TelegramResponse -> Map.Map Int Int -> Query  -- checking value of repeats and get message for sending
makeRepeatMessage newResponse mapUsers = [("text", Just messageForRepeate)]  
    where messageForRepeate = messageFor <> "\n Click on any button to set the value:"
          messageFor = maybe defaultMessage (\number -> 
                            "Number of message repeats: " <> (BC.pack . show) number) maybeNumber
          maybeNumber = getUsersValue (getUserID newResponse) mapUsers                  
          defaultMessage = "Number of message repeats: 1 (default value)\n\
                        \Click on any button to set the value:\n"            
             
makeCopyMessage :: TelegramResponse -> Query
makeCopyMessage newResponse =  [ ("from_chat_id", Just chatID)
                               , ("message_id", Just messageID)
                               ] 
    where chatID = getChatID newResponse
          messageID = getMessageID newResponse
       
         

getMessageContent :: TelegramResponse -> Query  -- get query string for answer
getMessageContent newResponse
    | isJust $ callback_query update  = [("text", Just "Setting new value")]
    | otherwise                       = parseMessageContent maybeMessage
    where parseMessageContent input = if isJust $ input >>= text
                                      then checkCommandMessage newResponse
                                      else makeCopyMessage newResponse 
          update = head . result $ newResponse
          maybeMessage = message update <|> channel_post update 
                             

getChatID :: TelegramResponse -> ChatID
getChatID newResponse  
    | isJust $ message update         = BC.pack . show . _id . chat $ fromJust $ message update
    | isJust $ channel_post update    = BC.pack . show . _id . chat $ fromJust $ channel_post update
    | isJust $ callback_query update  = BC.pack . show . id . _from $ fromJust $ callback_query update
    | otherwise                       = error "The bot cannot handle messages of this type"
    where update = head . result $ newResponse

getMessageID :: TelegramResponse -> MessageID
getMessageID newResponse 
    | isJust $ message update          = BC.pack . show . message_id $ fromJust $ message update
    | isJust $ channel_post update     = BC.pack . show . message_id $ fromJust $ channel_post update
    | isJust $ callback_query update   = BC.pack . show . id . _from $ fromJust $ callback_query update
    | otherwise                        = error "message don't reseived"        
    where update = head . result $ newResponse      

getMessageEntity :: TelegramResponse -> Query
getMessageEntity newResponse = [("entities", fmap (LBC.toStrict . encode . entities) maybeMessage)]
    where update = head . result $ newResponse
          maybeMessage = message update <|> channel_post update


getMessageCaptionEntity :: TelegramResponse -> Query
getMessageCaptionEntity newResponse = [("caption_entities", fmap (LBC.toStrict . encode . caption_entities) maybeMessage)]  
    where update = head . result $ newResponse
          maybeMessage = message update <|> channel_post update    


          