{-# LANGUAGE OverloadedStrings #-}
module GetRequest 
    ( getUpdate
    , sendMessage
    ) where

import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Simple


myTelegramToken :: BC.ByteString
myTelegramToken = "1283454588:AAEfW-xN5ftFrvOTOtSMDUbXKu4N1bmmTYk"
botTelegramHost :: BC.ByteString
botTelegramHost = "https://api.telegram.org/bot" 
--botTelegramPath :: BC.ByteString
--botTelegramPath = "/bot"      

buildRequest :: BC.ByteString -> BC.ByteString
                -> BC.ByteString -> BC.ByteString
buildRequest token host method = host <> token <> method 

getUpdate :: BC.ByteString
getUpdate = buildRequest myTelegramToken 
            botTelegramHost "/GetUpdates" 

sendMessage :: BC.ByteString
sendMessage = buildRequest myTelegramToken botTelegramHost 
              "/sendMessage?chat_id=614000958&text=test_hello"

testMessage :: BC.ByteString
testMessage = "?chat_id=614000958&text=test_hello"                    

