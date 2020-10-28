{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser (someFunc)
import Network.HTTP.Simple
import Network.HTTP.Types (Status(..))
import qualified Data.ByteString.Char8 as BC 
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
    response <- httpBS temp 
    upd <- getUpdate
    update <- httpBS . parseRequestThrow_ . BC.unpack $ upd
    let code = statusCode . getResponseStatus $ response
    let error = statusMessage . getResponseStatus $ response
    if code == 200
    then do
        putStrLn "saving request to file\n\n"
        -- for testing
        let jsonBody = getResponseBody update
        BC.writeFile "data.json" jsonBody
        --
    else print $ "request failed: code-" <> show code 
         <> "; message-" <> show error

chatID :: IO BC.ByteString
chatID = return "614000958"    

messageIO :: IO BC.ByteString
messageIO = do
    putStrLn "Input your message"
    mess <- BC.getLine
    return mess