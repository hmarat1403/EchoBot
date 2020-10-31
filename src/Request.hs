{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Request where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T 
import GHC.Generics
import Data.Aeson
import Data.Aeson.Utils
--import Network.HTTP.Simple

type Host = BC.ByteString
type Path = BC.ByteString
type Token = BC.ByteString

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
--instance ToJSON SendMessage   

data ReplyKeyboardMarkUp = ReplyKeyboardMarkUp
                           { keyBoard :: [[KeyboardButton]]
                           , one_time_keyboard :: Bool
                           } deriving (Show, Generic)
--instance ToJSON ReplyKeyboardMarkUp                                             
            
data KeyboardButton = KeyboardButton 
                      { textKB :: BC.ByteString
                      } deriving (Show, Generic)
--instance ToJSON KeyboardButton           

data GetUpdates = GetUpdates
                 { offset :: Int
                 , limit :: Int
                 , timeout :: Int
                 , allowed_updates :: [BC.ByteString]
                 } deriving (Show, Generic)
--instance ToJSON GetUpdates                   

data SendPhoto = SendPhoto 
                 { photoChat_id :: Int
                 , photo :: BC.ByteString
                 } deriving Show 
-- instance ToJSON SendPhoto where
--     toJSON (SendPhoto photoChat_id photo) 
--             = object [ "chat_id" .= photoChat_id
--                      , "photo" .= photo
--                      ]
data SendAnimation = SendAnimation
                     { animationChat_id :: Int
                     , animation :: BC.ByteString
                     } deriving Show
-- instance ToJSON SendAnimation where
--     toJSON (SendAnimation animationChat_id animation) 
--             = object [ "chat_id" .= animationChat_id
--                      , "animation" .= animation
--                      ]                           
data SendAudio = SendAudio
                 { audioChat_id :: Int
                 , audio :: BC.ByteString
                 } deriving Show
-- instance ToJSON SendAudio where
--     toJSON (SendAudio audioChat_id audio) 
--             = object [ "chat_id" .= audioChat_id
--                      , "audio" .= audio
--                      ]    
data SendContact = SendContact
                   { contactChat_id :: Int
                   , phone_number :: BC.ByteString
                   , contactFirst_name :: BC.ByteString
              --     , contactLast_name :: Maybe BC.ByteString
              --    , vcard :: Maybe BC.ByteString
                   } deriving Show
-- instance ToJSON SendContact where
--     toJSON (SendContact contactChat_id phone_number contactFirst_name) 
--             = object [ "chat_id" .= contactChat_id
--                      , "phone_number" .= phone_number
--                      , "first_name" .= contactFirst_name
--                      ]        
data SendDocument = SendDocument
                    { documentChat_id :: Int
                    , document :: BC.ByteString
                    } deriving Show
-- instance ToJSON SendDocument where
--     toJSON (SendDocument documentChat_id document) 
--             = object [ "chat_id" .= documentChat_id
--                      , "document" .= document
--                      ]       
data SendVideo = SendVideo
                 { videoChat_id :: Int
                 , video :: BC.ByteString
                 } deriving Show
-- instance ToJSON SendVideo where
--     toJSON (SendVideo videoChat_id video) 
--             = object [ "chat_id" .= videoChat_id
--                      , "video" .= video
--                      ]        
data SendVoice = SendVoice
                 { voiceChat_id :: Int
                 , voice :: BC.ByteString
                 } deriving Show
-- instance ToJSON SendVoice where
--     toJSON (SendVoice voiceChat_id voice) 
--             = object [ "chat_id" .= voiceChat_id
--                      , "voice" .= voice
--                      ]     
data SendSticker = SendSticker
                   { stickerChat_id :: Int
                   , sticker :: BC.ByteString -- file_id
                   } deriving Show
-- instance ToJSON SendSticker where
--     toJSON (SendSticker stickerChat_id sticker) 
--             = object [ "chat_id" .= stickerChat_id
--                      , "photo" .= sticker
--                      ]                                                                                                                                                                                                         
botTelegramHost :: Host
botTelegramHost = "https://api.telegram.org" 
botTelegramPath ::Path
botTelegramPath = "/bot"  
    

buildRequest :: Host -> Path -> Token -> BC.ByteString -> BC.ByteString
buildRequest host path token  method = host <> path <> token 
                                        <> method

getUpdate :: IO BC.ByteString
getUpdate = do 
    token <- readToken
    let update = buildRequest botTelegramHost botTelegramPath 
                 token "/GetUpdates" 
    return update 

-- for testing
prepareMessage :: IO BC.ByteString -> IO BC.ByteString -> IO BC.ByteString
prepareMessage chatID messageIO = do
    token <- readToken
    id <- chatID
    message <- messageIO
    let reg = buildRequest botTelegramHost botTelegramPath  
              token "/sendSticker" 
    let request = reg <> "?chat_id=" <> id <> "&sticker=" <> message  
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



