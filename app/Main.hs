{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser (someFunc)
import Data.Aeson
import TelegramAPI
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Simple
import Network.HTTP.Types (Status(..))
import qualified Data.ByteString.Char8 as BC 
import qualified Data.ByteString.Lazy.Char8 as LBC 
--import Data.Aeson.Lens
--import Control.Lens
import GetRequest (prepareMessage, getUpdate)


main :: IO ()
main = do
    message <- prepareMessage chatID messageIO
    let temp = parseRequestThrow_ . BC.unpack $ message
    -- for testing
    let forFile = show temp
    writeFile "request.json" forFile
    -- 
    response <- httpLBS temp 
    upd <- getUpdate
    update <- httpLBS . parseRequestThrow_ . BC.unpack $ upd
    let code = statusCode . getResponseStatus $ response
    let error = statusMessage . getResponseStatus $ response
    if code == 200
    then do
        putStrLn "saving request to file\n\n"
        let jsonBody = getResponseBody update
        -- for testing
        L.writeFile "data.json" jsonBody
        --
    else print $ "request failed: code-" <> show code 
         <> "; message-" <> show error

test :: IO ()
test = do
    jsonBody <- L.readFile "data.json" 
   -- let telRes = encode jsonBody
    let telegramResponse = eitherDecode jsonBody
    case telegramResponse of 
        Left error -> print $ "cannot parse" <> error
        Right res -> do 
            let telegramResults = result <$> res
            printResults telegramResults
    

chatID :: IO BC.ByteString
chatID = return "614000958"    

messageIO :: IO BC.ByteString
messageIO = do
    putStrLn "Input your message"
    mess <- BC.getLine
    return mess