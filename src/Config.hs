module Config
       ( readToken
       , telegramOffset
       , telegramLimit
       , telegramTimeout
       , telegramAllowUpdates
       )  where

import qualified Data.ByteString.Char8 as BC 

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