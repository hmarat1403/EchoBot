{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser (someFunc)
import Data.Aeson
import TelegramAPI ( TelegramResponse(result)
                   , Update (message)
                   , getMessageContent
                   , getPrefix
                   , getSendingMethod
                   , getMessageChatID )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Simple
import Network.HTTP.Types (Status(..))
import qualified Data.ByteString.Char8 as BC 
import qualified Data.ByteString.Lazy.Char8 as LBC 
--import Data.Aeson.Lens
--import Control.Lens
import Request (prepareMessage, getUpdate)
import TelegramAPI ()

main :: IO ()
main = do
    upd <- getUpdate
    update <- httpLBS . parseRequestThrow_ . BC.unpack $ upd
    let code = statusCode . getResponseStatus $ update
    let error = statusMessage . getResponseStatus $ update
    if code == 200
    then do
        putStrLn "saving request to file\n\n"
        let jsonBody = getResponseBody update
        -- for testing
        L.writeFile "data.json" jsonBody
    else print $ "request failed: code-" <> show code 
              <> "; message-" <> show error

test :: IO ()
test = do
    jsonBody <- L.readFile "data.json" 
   -- let telRes = encode jsonBody
    let telegramResponse = eitherDecode jsonBody
    case telegramResponse of 
        Left error -> print $ "cannot parse: " <> error
        Right res -> do 
            let telRes = last . result $ res
            let chat = getMessageChatID . message $ telRes
            let cont = getMessageContent . message $ telRes
            let met = getSendingMethod . message $ telRes
            let pref = getPrefix . message $ telRes
            request <- prepareMessage chat pref cont met 
            send <- httpLBS . parseRequestThrow_ . BC.unpack $ request 
            print "sending"
            

messageIO :: IO BC.ByteString
messageIO = do
    putStrLn "Sending message"
    let mess = fileID
    return mess

fileID :: BC.ByteString
fileID = "AgACAgIAAxkBAAM9X519C-n0Eiv5oB_44biVc9RtFSEAAi-0MRvah-lIeleB6GflvE3wVWCaLgADAQADAgADeQADXzsAAhsE"
 --   "AgACAgIAAxkBAAM9X519C-n0Eiv5oB_44biVc9RtFSEAAi-0MRvah-lIeleB6GflvE3wVWCaLgADAQADAgADbQADYTsAAhsE"