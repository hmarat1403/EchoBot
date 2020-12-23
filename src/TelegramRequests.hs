{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module TelegramRequests where

import GHC.Generics ( Generic )
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Simple (addToRequestQueryString, httpLBS, parseRequest_, Request)
{- import Parser (getMessageCaptionEntity
              , getMessageEntity
              , getMessageContent
              , SendingMethod
              , makeRepeatMessage
              , getChatID
              , getSendingMethod
              , checkNullUpdate
              )
import Config ( readToken
              , telegramLimit
              , telegramTimeout
              , defaultKeyboard
              , telegramAllowUpdates
              ) -}
import Data.Maybe (fromJust)
import Control.Applicative (Alternative((<|>)))

type Host = BC.ByteString
type Path = BC.ByteString
type Token = BC.ByteString
type TelRequest = BC.ByteString
type TelOffset = Int
type TelLimit = Int
type TelTimeout = Int
type TelAllowedUpdates = [T.Text]
type UpdatesParametrs = BC.ByteString 
type SendMessage = BC.ByteString
type CopyMessage = BC.ByteString
type GetUpdates = BC.ByteString

data APIMethod = SendMessage | CopyMessage | GetUpdates
             
data StringForParsingRequest = StringForParsingRequest 
                         { host :: Host
                         , path :: Path
                         , method :: APIMethod
                         } deriving Generic
instance Show StringForParsingRequest where
    show httpString = BC.unpack (host httpString <> "/" <> path httpString <> "/" <> showMethod httpString) 
        where showMethod req = case method req of
                SendMessage -> "SendMessage"
                CopyMessage -> "CopyMessage"
                GetUpdates  -> "GetUpdates"

botTelegramHost :: Host
botTelegramHost = "https://api.telegram.org" 
botTelegramPath :: Token -> Path
botTelegramPath = ("bot" <>) 
{- allowUpdates :: TelAllowedUpdates
allowUpdates = telegramAllowUpdates -}

buildStringForParsing :: Host -> Path -> APIMethod -> StringForParsingRequest
buildStringForParsing apiHost apiPath apiMethod = StringForParsingRequest
                                         { host = apiHost
                                         , path = apiPath
                                         , method = apiMethod }
                                     

stringForTelegram :: Token -> APIMethod -> StringForParsingRequest
stringForTelegram token = buildStringForParsing botTelegramHost (botTelegramPath token)


parseRequestForTelegram :: StringForParsingRequest -> Request
parseRequestForTelegram = parseRequest_ . show 