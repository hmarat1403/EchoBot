{-# LANGUAGE OverloadedStrings #-}
module Request ( buildRequest
               , updatesParametrs
               , getUpdate
               , sendMessage
               , prepareMessage
               , makeUpdateRequest
               ) where

import Parser (getMessageCaptionEntity
              , getMessageEntity
              , getMessageContent
              , SendingMethod
              , makeRepeatMessage
              , getChatID
              , getSendingMethod
              , checkNullUpdate
              )
import Config ( readToken
              , telegramLimit
              , telegramTimeout
              , defaultKeyboard
              , telegramAllowUpdates
              )
import Users (readMapFromFile)  
import TelegramAPI (TelegramResponse ())   
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBS (toStrict)         
import Network.HTTP.Simple (addToRequestQueryString, httpLBS, parseRequest_, Request)
import Data.Aeson (encode)
import Control.Monad (unless)


type Host = BC.ByteString
type Path = BC.ByteString
type Token = BC.ByteString
type TelRequest = BC.ByteString
type TelOffset = Int
type TelLimit = Int
type TelTimeout = Int
type TelAllowedUpdates = [T.Text]
type UpdatesParametrs = BC.ByteString 

botTelegramHost :: Host
botTelegramHost = "https://api.telegram.org" 
botTelegramPath :: Path
botTelegramPath = "/bot"  
allowUpdates :: TelAllowedUpdates
allowUpdates = telegramAllowUpdates


buildRequest :: Host -> Path -> Token -> TelRequest
buildRequest host path token = host <> path <> token 

updatesParametrs :: TelLimit -> TelTimeout-> TelOffset -> UpdatesParametrs  -- запрос без TelAllowedUpdates
updatesParametrs telLimit telTimeout telOffset = 
    "?offset=" <> telOffsetBCString <> "&limit=" <> telLimitBCString <> "&timeout=" <> telTimeoutBCString 
    where telOffsetBCString = BC.pack . show $ telOffset
          telTimeoutBCString = BC.pack . show $ telTimeout
          telLimitBCString = BC.pack . show $ telLimit
          
getUpdate :: TelOffset -> Token -> TelRequest
getUpdate lastUpdateID token =  let body = buildRequest botTelegramHost botTelegramPath token 
                                    suffics = updatesParametrs telegramLimit telegramTimeout (lastUpdateID + 1) 
                                in body <> "/getUpdates" <> suffics          
    
makeUpdateRequest :: TelRequest -> Request
makeUpdateRequest telRequest = 
    let requestWithoutAllow = parseRequest_ . BC.unpack $ telRequest
    in addToRequestQueryString [("allowed_updates", Just $ (LBS.toStrict . encode) allowUpdates)] requestWithoutAllow    
   

prepareMessage :: SendingMethod -> Token -> TelRequest
prepareMessage method token = let reg = buildRequest botTelegramHost botTelegramPath token
                              in reg <> method 
 

sendMessage :: TelegramResponse -> IO ()
sendMessage decodeUpdate = do 
    token <- readToken
    unless (checkNullUpdate decodeUpdate) 
        (do let chat = getChatID decodeUpdate 
            let cont = getMessageContent decodeUpdate
            let meth = getSendingMethod decodeUpdate 
            let ent = getMessageEntity decodeUpdate 
            let cap_ent = getMessageCaptionEntity decodeUpdate
            let request = (parseRequest_ . BC.unpack) (prepareMessage meth token)
            let requestForChat = addToRequestQueryString [("chat_id", Just chat)] request
            if (snd . head $ cont) /= Just BC.empty
            then do 
                let requestWithContent = addToRequestQueryString (cont <> ent <> cap_ent) requestForChat
                httpLBS requestWithContent
                return ()
            else do 
                mapOfUsers <- readMapFromFile "Users.txt"
                let contForRepeat = makeRepeatMessage decodeUpdate mapOfUsers
                let requestWithContent = addToRequestQueryString contForRepeat requestForChat
                let requestWithKeyboard = 
                     addToRequestQueryString [ ("reply_markup"
                                             , Just $ (LBS.toStrict . encode) defaultKeyboard)
                                             ] requestWithContent
                httpLBS requestWithKeyboard
                return ()
        )   
