module Config
       ( readToken
       , telegramOffset
       , telegramLimit
       , telegramTimeout
       , telegramAllowUpdates
       , telegramUsers
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
telegramOffset = 793579169   

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
  