{-# LANGUAGE OverloadedStrings #-}
module Main where

--import Parser (someFunc)
import Data.Aeson ( eitherDecode )
import TelegramAPI ( TelegramResponse (result)
                   , Update (message))
import Parser ( getMessageContent
              , getPrefix
              , getSendingMethod
              , getMessageChatID 
              , getLastUpdateNumber )
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Simple (Response,  getResponseBody
                           , getResponseStatus
                           , httpLBS
                           , parseRequestThrow_ )
import Network.HTTP.Types (Status(..))
import qualified Data.ByteString.Char8 as BC 
import Request (prepareMessage, getUpdate)
import Data.IORef ( writeIORef, newIORef, readIORef )
import Control.Monad (forever)
import Config (telegramOffset)


main :: IO ()
main = do
    startNumber <- newIORef telegramOffset  
    forever $ do
        updRequest <- getUpdate . readIORef $ startNumber
        update <- httpLBS . parseRequestThrow_ . BC.unpack $ updRequest
        let code = statusCode . getResponseStatus $ update
        let error = statusMessage . getResponseStatus $ update
        if code == 200
        then do 
            num <- getLastUpdateNumber update
            a <- readIORef startNumber
            if num <= a
            then return ()
            else do 
                writeIORef startNumber num
                sendMessage update                                            
        else print $ "request failed: code-" <> show code 
                  <> "; message-" <> show error

sendMessage :: Response L.ByteString -> IO ()
sendMessage reseivingBC = do
    let jsonBody = getResponseBody reseivingBC
    let telegramResponse = eitherDecode jsonBody
    case telegramResponse of 
        Left error -> print $ "cannot parse: " <> error
        Right res -> do 
            if null (result res) 
            then return ()
            else do
                let telRes = head . result $ res
                let chat = getMessageChatID . message $ telRes
                let cont = getMessageContent . message $ telRes
                let met = getSendingMethod . message $ telRes
                let pref = getPrefix . message $ telRes
                request <- prepareMessage chat pref met 
                httpLBS . parseRequestThrow_ $ ((BC.unpack request) 
                            <> cont)
                return ()
           
