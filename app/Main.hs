{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser (someFunc)
import Network.HTTP.Simple
import Network.HTTP.Types (Status(..))
import qualified Data.ByteString.Char8 as BC 
import Data.Aeson.Lens
import Control.Lens
import GetRequest (sendMessage, getUpdate)


main :: IO ()
main = do
    response <- httpBS . parseRequestThrow_ . BC.unpack $ sendMessage
    update <- httpBS . parseRequestThrow_ . BC.unpack $ getUpdate
    let code = statusCode . getResponseStatus $ response
    let error = statusMessage . getResponseStatus $ response
    if code == 200
    then do
        putStrLn "saving request to file\n\n"
        print update
    else print $ "request failed: code-" <> show code 
         <> "; message-" <> show error

