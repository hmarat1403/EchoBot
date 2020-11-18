{-# LANGUAGE OverloadedStrings #-}
module Request ( buildRequest
               , getUpdatesParametrs
               , getUpdate
               , sendMessage
               , prepareMessage
               ) where

import qualified Data.ByteString.Char8 as BC
import Parser ( getPrefix
              , getSendingMethod
              , getMessageContent
              , getMessageChatID
              , PrefixMessage
              , SendingMethod
              , ChatID )
import Config ( telegramAllowUpdates
              , readToken
              , telegramLimit
              , telegramTimeout)
import Network.HTTP.Simple (httpLBS, parseRequest_)
import TelegramAPI (message, TelegramResponse (result))


type Host = BC.ByteString
type Path = BC.ByteString
type Token = BC.ByteString
type TelRequestBody = BC.ByteString
type TelOffset = Int
type TelLimit = Int
type TelTimeout = Int
type TelAllowedUpdates = BC.ByteString
type GetUpdatesParametrs = BC.ByteString 

botTelegramHost :: Host
botTelegramHost = "https://api.telegram.org" 
botTelegramPath ::Path
botTelegramPath = "/bot"  
telegramToken :: IO Token
telegramToken = readToken

buildRequest :: Host -> Path -> Token -> TelRequestBody
buildRequest host path token = host <> path <> token 

getUpdatesParametrs :: TelLimit -> TelTimeout-> TelAllowedUpdates 
                       -> TelOffset -> GetUpdatesParametrs
getUpdatesParametrs telLimit telTimeout telAllowedUpdates telOffset = 
    "?offset=" <> telOffsetBCString <> "&limit=" <> telLimitBCString 
    <> "&timeout=" <> telTimeoutBCString <> "&allowed_updates=" <> telAllowedUpdates
    where telOffsetBCString = BC.pack . show $ telOffset
          telTimeoutBCString = BC.pack . show $ telTimeout
          telLimitBCString = BC.pack . show $ telLimit

getUpdate :: IO TelOffset -> IO BC.ByteString
getUpdate lastUpdateID = do 
    token <- telegramToken
    updateID <- lastUpdateID
    let body = buildRequest botTelegramHost botTelegramPath token 
    let suffics = getUpdatesParametrs telegramLimit telegramTimeout 
                    telegramAllowUpdates (updateID + 1)
    let update = body <> "/getUpdates" <> suffics                        
    return update 

prepareMessage :: ChatID -> PrefixMessage ->  
                  SendingMethod -> IO BC.ByteString
prepareMessage chatID prefix method = do
    token <- telegramToken
    let reg = buildRequest botTelegramHost botTelegramPath  
              token
    let request = reg <> method <> "?chat_id=" <> chatID 
                  <> prefix  
    return request

sendMessage :: TelegramResponse -> IO ()
sendMessage decodeUpdate = do
    if null (result decodeUpdate) 
    then return ()
    else do
        let telRes = head . result $ decodeUpdate
        let chat = getMessageChatID . message $ telRes
        let cont = getMessageContent . message $ telRes
        let met = getSendingMethod . message $ telRes
        let pref = getPrefix . message $ telRes
        request <- prepareMessage chat pref met 
     --   print $ BC.unpack request <> cont
        httpLBS . parseRequest_ $ (BC.unpack request <> cont)
        return ()    

{- data TelegramRequest = TelegramRequest
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
                 { offset :: TelOffset
                 , limit :: TelLimit
                 , timeout :: TelTimeout
                 , allowed_updates :: [TelAllowedUpdates]
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
                   } deriving Show -}

