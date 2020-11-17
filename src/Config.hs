{-# LANGUAGE OverloadedStrings #-}

module Config
       ( readToken
       , telegramOffset
       , telegramLimit
       , telegramTimeout
       , telegramAllowUpdates
       , telegramUsers
       , defaultNumberOfMessages
       , helpMessage
       , defaultHelpMessage
       , defaultRepeateMessage
       )  where

import qualified Data.ByteString.Char8 as BC 
import qualified Data.Map as Map
import Users ( readMapFromFile )
import System.Directory ( doesFileExist )

readToken :: IO BC.ByteString
readToken = do
    string <- readFile "Data.txt" 
    let token = BC.pack string 
    return token 

telegramOffset :: Int
telegramOffset = 0  

telegramLimit :: Int
telegramLimit = 10

telegramTimeout :: Int
telegramTimeout = 25

telegramAllowUpdates :: BC.ByteString
telegramAllowUpdates = BC.empty

telegramUsers :: IO (Map.Map Int Int)
telegramUsers = do 
    existFile <- doesFileExist "Users.txt"
    if existFile
    then readMapFromFile "Users.txt"
    else return Map.empty

defaultNumberOfMessages :: Int
defaultNumberOfMessages = 1      

helpMessage :: IO BC.ByteString
helpMessage = do 
    existFile <- doesFileExist "Help.txt"
    if existFile
    then BC.readFile "Help.txt"
    else return defaultHelpMessage

defaultHelpMessage :: BC.ByteString
defaultHelpMessage = "I am echo-bot. I can send back the received messages\n\
                  \I accept commands /help and /repeate \n\
                  \/help displays information about me\n\
                  \/repeate displays information about the number of\n\
                  \repeating messages and give you the opportunity\n\
                  \to change this number in the range from up to 5" 
defaultRepeateMessage :: BC.ByteString
defaultRepeateMessage = "Number of message repeats: 1 (default value)"                  