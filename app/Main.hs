{-# LANGUAGE OverloadedStrings #-}
module Main where

--import Parser (someFunc)
import Data.Aeson ( eitherDecode )
import TelegramAPI ( TelegramResponse (result)
                   , Update (message))
import Parser ( getLastUpdateNumber, getDecodeUpdate, getUserID, checkUser )
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Simple ( getResponseStatus
                           , httpLBS
                           , parseRequestThrow_ )
import Network.HTTP.Types (Status(..))
import qualified Data.ByteString.Char8 as BC 
import Request (prepareMessage, getUpdate, sendMessage)
import Data.IORef ( writeIORef, newIORef, readIORef )
import Control.Monad (forever)
import Config (telegramOffset, telegramUsers)


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
            decodedUpdate <- getDecodeUpdate update
            let num = getLastUpdateNumber decodedUpdate
            a <- readIORef startNumber
            if num <= a
            then return ()
            else do 
                writeIORef startNumber num
                sendMessage decodedUpdate                                            
        else print $ "request failed: code-" <> show code 
                  <> "; message-" <> show error


           
