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
       , defaultKeyboard
       )  where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC 
import qualified Data.Map as Map
import System.Directory ( doesFileExist )
import TelegramAPI (InlineKeyboardButton (..), InlineKeyboardMarkUp (..))
import Users (readMapFromFile)

readToken :: IO BC.ByteString
readToken = do
    string <- readFile "Data.txt" 
    let token = BC.pack string 
    return token 

telegramOffset :: Int
telegramOffset = 793579284 

telegramLimit :: Int
telegramLimit = 10

telegramTimeout :: Int
telegramTimeout = 25

telegramAllowUpdates :: [T.Text]
telegramAllowUpdates = ["message", "channel_post", "callback_query"]

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
                  \I accept commands /help and /repeat \n\
                  \/help displays information about me\n\
                  \/repeat displays information about the number of\n\
                  \repeating messages and give you the opportunity\n\
                  \to change this number in the range from up to 5" 

defaultKeyboard :: InlineKeyboardMarkUp
defaultKeyboard = InlineKeyboardMarkUp { inline_keyboard = 
                                      [ [oneButton]
                                      , [twoButton]
                                      , [threeButton]
                                      , [forButton]
                                      , [fiveButton]
                                      ]}
oneButton :: InlineKeyboardButton
oneButton = InlineKeyboardButton { text = "1", callback_data = "1" }
twoButton :: InlineKeyboardButton
twoButton = InlineKeyboardButton { text = "2", callback_data = "2" }
threeButton :: InlineKeyboardButton
threeButton = InlineKeyboardButton { text = "3", callback_data = "3" }
forButton :: InlineKeyboardButton
forButton = InlineKeyboardButton { text = "4", callback_data = "4" }
fiveButton :: InlineKeyboardButton
fiveButton = InlineKeyboardButton { text = "5", callback_data = "5" }
