{-# LANGUAGE OverloadedStrings #-}
module Main where

import Users (addUserToMap,  checkUser, getUserID, writeMapToFile )
import Data.Aeson ( eitherDecode )
import TelegramAPI ( TelegramResponse (result)
                   , Update (message))
import Parser ( getLastUpdateNumber, getDecodeUpdate )
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Simple ( getResponseStatus
                           , httpLBS
                           , parseRequestThrow_ )
import Network.HTTP.Types (Status(..))
import qualified Data.ByteString.Char8 as BC 
import Request (prepareMessage, getUpdate, sendMessage)
import Data.IORef ( writeIORef, newIORef, readIORef )
import Control.Monad (forever, unless, when )
import Config (telegramOffset, telegramUsers)
import Data.Maybe (fromJust)


main :: IO ()
main = do
    startNumber <- newIORef telegramOffset  
    usersList <- newIORef telegramUsers
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
                let maybeID = getUserID decodedUpdate
                listOfUsersIO <- readIORef usersList
                listOfUsers <- listOfUsersIO
             --   print listOfUsers
                unless (checkUser maybeID listOfUsers) (do
                    let newMap = addUserToMap (fromJust maybeID) listOfUsers
                    writeIORef usersList . return $ newMap
                    writeMapToFile "Users.txt" $ newMap
                    print newMap)
                writeIORef startNumber num
                sendMessage decodedUpdate                                            
        else print $ "request failed: code-" <> show code 
                  <> "; message-" <> show error


           
