{-# LANGUAGE OverloadedStrings #-}
module GetRequest 
    ( getUpdate
    , prepareMessage
    ) where

import qualified Data.ByteString.Char8 as BC
--import Network.HTTP.Simple


botTelegramHost :: BC.ByteString
botTelegramHost = "https://api.telegram.org/bot" 
--botTelegramPath :: BC.ByteString
--botTelegramPath = "/bot"      

buildRequest :: BC.ByteString -> BC.ByteString
                -> BC.ByteString -> BC.ByteString
buildRequest host token  method = host <> token <> method

getUpdate :: IO BC.ByteString
getUpdate = do 
    token <- readToken
    let update = buildRequest botTelegramHost token "/GetUpdates" 
    return update 

prepareMessage :: IO BC.ByteString -> IO BC.ByteString -> IO BC.ByteString
prepareMessage chatID messageIO = do
    token <- readToken
    id <- chatID
    message <- messageIO
    let reg = buildRequest botTelegramHost token "/sendMessage" 
    let request = reg <> "?chat_id=" <> id <> "&text=" <> message  
    return request

readToken :: IO BC.ByteString
readToken = do
    string <- readFile "Data.txt" 
    let token = BC.pack string 
    return token   

    



