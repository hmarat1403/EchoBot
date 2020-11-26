{-# LANGUAGE OverloadedStrings #-}
module Main where

import Users (addUserToMap,  checkUser, getUserID, writeMapToFile, getUsersValue, changeUserInMap )
import Parser ( getLastUpdateNumber, getDecodeUpdate, checkCallbackQuery )
import Network.HTTP.Simple ( getResponseStatus, httpLBS )
import Network.HTTP.Types (Status(..))
import Request (getUpdate, sendMessage, updateRequest)
import Data.IORef ( writeIORef, newIORef, readIORef )
import Control.Monad (forever, forM_ )
import Config (telegramAllowUpdates, telegramOffset, telegramUsers)
import Data.Maybe (fromJust, isJust)


main :: IO ()
main = do
    startNumber <- newIORef telegramOffset  
    usersList <- newIORef telegramUsers
    forever $ do
        request <- getUpdate . readIORef $ startNumber
        update <- httpLBS $ updateRequest request telegramAllowUpdates
        let code = statusCode . getResponseStatus $ update
        let error = statusMessage . getResponseStatus $ update
        if code == 200
        then do 
            let decodedUpdate = getDecodeUpdate update
            let num = getLastUpdateNumber decodedUpdate
            a <- readIORef startNumber
            if num <= a
            then return ()
            else do 
                let maybeID = getUserID decodedUpdate
                listOfUsersIO <- readIORef usersList
                listOfUsers <- listOfUsersIO
                if checkUser maybeID listOfUsers
                then (do
                     let checkCQ = checkCallbackQuery decodedUpdate
                     if isJust checkCQ
                     then (do 
                        let repeating = fromJust checkCQ
                        let newMap = changeUserInMap repeating (fromJust maybeID) listOfUsers
                        writeIORef usersList . return $ newMap
                        writeMapToFile "Users.txt" newMap                        
                        sendMessage decodedUpdate)
                     else (do    
                        let repeating = fromJust $ getUsersValue maybeID listOfUsers
                        forM_ [1..repeating] $ \_ -> sendMessage decodedUpdate)
                     )
                else (do
                    let newMap = addUserToMap (fromJust maybeID) listOfUsers
                    writeIORef usersList . return $ newMap
                    writeMapToFile "Users.txt" newMap
                    sendMessage decodedUpdate)
                writeIORef startNumber num
        else print $ "request failed: code-" <> show code 
                  <> "; message-" <> show error


           
