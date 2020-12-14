{-# LANGUAGE OverloadedStrings #-}
module Request ( buildRequest
               , updatesParametrs
               , updateRequest
               , getUpdate
               , sendMessage
               , prepareMessage
               ) where

import Parser (getMessageCaptionEntity
              , getMessageEntity
              , getMessageContent
              , SendingMethod
              , makeRepeatMessage
              , getMessageChatID
              , getSendingMethod
              , checkNullUpdate
              )
import Config ( readToken
              , telegramLimit
              , telegramTimeout
              , defaultKeyboard
              )
import Users (readMapFromFile)  
import TelegramAPI (TelegramResponse ())   
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBS (toStrict)         
import Network.HTTP.Simple (addToRequestQueryString, httpLBS, parseRequest_, Request)
import Data.Aeson (encode)
import Network.HTTP.Conduit ( urlEncodedBody )
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
botTelegramPath ::Path
botTelegramPath = "/bot"  
telegramToken :: IO Token
telegramToken = readToken

buildRequest :: Host -> Path -> Token -> TelRequest
buildRequest host path token = host <> path <> token 

updatesParametrs :: TelLimit -> TelTimeout-> TelOffset -> UpdatesParametrs  -- запрос без TelAllowedUpdates
updatesParametrs telLimit telTimeout telOffset = 
    "?offset=" <> telOffsetBCString <> "&limit=" <> telLimitBCString 
    <> "&timeout=" <> telTimeoutBCString
    where telOffsetBCString = BC.pack . show $ telOffset
          telTimeoutBCString = BC.pack . show $ telTimeout
          telLimitBCString = BC.pack . show $ telLimit

getUpdate :: IO TelOffset -> IO TelRequest
getUpdate lastUpdateID = do 
    token <- telegramToken
    updateID <- lastUpdateID
    let body = buildRequest botTelegramHost botTelegramPath token 
    let suffics = updatesParametrs telegramLimit telegramTimeout (updateID + 1)
    let update = body <> "/getUpdates" <> suffics          
    return update 

updateRequest :: TelRequest -> TelAllowedUpdates -> Request
updateRequest updRequest allowUpdates = 
    let request1 = parseRequest_ . BC.unpack $ updRequest
    in  urlEncodedBody [("allowed_updates", (LBS.toStrict . encode) allowUpdates)] request1
    
prepareMessage :: SendingMethod -> IO TelRequest
prepareMessage method = do
    token <- telegramToken
    let reg = buildRequest botTelegramHost botTelegramPath token
    let request = reg <> method 
    return request
  

sendMessage :: TelegramResponse -> IO ()
sendMessage decodeUpdate = do 
    unless (checkNullUpdate decodeUpdate) 
        (do let chat = getMessageChatID decodeUpdate 
            let cont = getMessageContent decodeUpdate
            let meth = getSendingMethod decodeUpdate 
            let ent = getMessageEntity decodeUpdate 
            let cap_ent = getMessageCaptionEntity decodeUpdate
            request <- fmap (parseRequest_ . BC.unpack) (prepareMessage meth)
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
                     urlEncodedBody [ ("reply_markup"
                                    , (LBS.toStrict . encode) defaultKeyboard)
                                    ] requestWithContent
                httpLBS requestWithKeyboard
                return ()
        )   
