{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Request where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T 
import TelegramAPI  (PrefixMessage, SendingMethod,  ChatID
                    , ReseivedMessage
                    , getMessageContent
                    , getMessageChatID )
import GHC.Generics
import Data.Aeson
import Data.Aeson.Utils
import Data.Int (Int)
--import Network.HTTP.Simple

type Host = BC.ByteString
type Path = BC.ByteString
type Token = BC.ByteString
type TelRequestBody = BC.ByteString

data TelegramRequest = TelegramRequest
                       { hostTel :: Host
                       , pathTel :: Path
                       , tokenTel :: Token
                       , getUpd :: Maybe GetUpdates
                       , sendMess :: Maybe SendMessage
                       , sendRhoto :: Maybe SendPhoto
                       , sendAnimation :: Maybe SendAnimation
                       , sendAudio :: Maybe SendAudio 
                       , sendDocument :: Maybe SendDocument
                       , sendVideo :: Maybe SendVideo 
                       , sendVoice :: Maybe SendVoice 
                       , sendSticker :: Maybe SendSticker
                       , sendContact :: Maybe SendContact 
                       } deriving (Show, Generic)
data SendMessage = SendMessage
                   { chat_id :: Int
                   , text :: BC.ByteString
                   , reply_markup :: ReplyKeyboardMarkUp
                   } deriving (Show, Generic)


data ReplyKeyboardMarkUp = ReplyKeyboardMarkUp
                           { keyBoard :: [[KeyboardButton]]
                           , one_time_keyboard :: Bool
                           } deriving (Show, Generic)
                                           
            
data KeyboardButton = KeyboardButton 
                      { textKB :: BC.ByteString
                      } deriving (Show, Generic)
       

data GetUpdates = GetUpdates
                 { offset :: Int
                 , limit :: Int
                 , timeout :: Int
                 , allowed_updates :: [BC.ByteString]
                 } deriving (Show, Generic)
         

data SendPhoto = SendPhoto 
                 { photoChat_id :: Int
                 , photo :: BC.ByteString -- file_id
                 , photoCaption :: BC.ByteString
                 } deriving Show 

data SendAnimation = SendAnimation
                     { animationChat_id :: Int
                     , animation :: BC.ByteString
                     , animationCaption :: BC.ByteString
                     } deriving Show
                        
data SendAudio = SendAudio
                 { audioChat_id :: Int
                 , audio :: BC.ByteString
                 , audioCaption :: BC.ByteString
                 } deriving Show
  
data SendContact = SendContact
                   { contactChat_id :: Int
                   , phone_number :: BC.ByteString
                   , contactFirst_name :: BC.ByteString
                   } deriving Show
     
data SendDocument = SendDocument
                    { documentChat_id :: Int
                    , document :: BC.ByteString
                    , documentCaption :: BC.ByteString
                     } deriving Show
 
data SendVideo = SendVideo
                 { videoChat_id :: Int
                 , video :: BC.ByteString
                 , videoCaption :: BC.ByteString
                 } deriving Show
     
data SendVoice = SendVoice
                 { voiceChat_id :: Int
                 , voice :: BC.ByteString
                 , voiceCaption :: BC.ByteString
                 } deriving Show
   
data SendSticker = SendSticker
                   { stickerChat_id :: Int
                   , sticker :: BC.ByteString -- file_id
                   } deriving Show
                                                                                                                                                                                                       
botTelegramHost :: Host
botTelegramHost = "https://api.telegram.org" 
botTelegramPath ::Path
botTelegramPath = "/bot"  

    
buildRequest :: Host -> Path -> Token -> TelRequestBody
buildRequest host path token = host <> path <> token 

getUpdate :: IO BC.ByteString
getUpdate = do 
    token <- readToken
    let update = buildRequest botTelegramHost botTelegramPath 
                 token <> "/GetUpdates" 
    return update 

-- for testing
prepareMessage :: ChatID -> PrefixMessage -> ReseivedMessage -> 
                  SendingMethod -> IO BC.ByteString
prepareMessage chatID prefix message method = do
    token <- readToken
    let reg = buildRequest botTelegramHost botTelegramPath  
              token
    let request = reg <> method <> "?chat_id=" <> chatID 
                  <> prefix <> message  
    return request

readToken :: IO Token
readToken = do
    string <- readFile "Data.txt" 
    let token = BC.pack string 
    return token   

stickerForTest :: SendSticker
stickerForTest = SendSticker { stickerChat_id = 614000958
                      , sticker = "CAACAgIAAxkBAAM2X5w9OCBV-qkirJ1TQZzvKatBMB4AAskBAAJWnb0KddhwxIgZLo0bBA"
                      }    



