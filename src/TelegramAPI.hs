{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module TelegramAPI where

import qualified Data.Text as T
import GHC.Generics (Generic) 
import Data.Aeson
    ( Value(Object), FromJSON(parseJSON), (.:), (.:?) )

-- telegram responce type
data TelegramResponse = Response 
                        { ok :: Bool
                        , result :: [Update]
                        } deriving (Show, Generic)
instance FromJSON TelegramResponse             

-- last 24 hours updates list type
data Update = Update
              { update_id :: Int
              , message :: Maybe Message 
              , inline_query :: Maybe InlineQuery
              , channel_post :: Maybe Message
              } deriving (Show, Generic)
instance FromJSON Update
data Message = Message
               { message_id :: Int
               , from :: Maybe User
               , chat :: Chat
               , date :: Int
               , text :: Maybe T.Text
               , entities :: Maybe [MessageEntity]
               , animation :: Maybe Animation
               , audio :: Maybe Audio
               , document :: Maybe Document
               , photo :: Maybe [PhotoSize]
               , sticker :: Maybe Sticker
               , video :: Maybe Video
               , video_note :: Maybe VideoNote
               , voice :: Maybe Voice
               , caption :: Maybe T.Text
               , contact :: Maybe Contact
               } deriving (Show, Generic)
instance FromJSON Message 
data Chat = Chat
            { chat_id :: Int
            , chat_Type :: T.Text
            , chatTitle :: Maybe T.Text
            , chat_FirstName :: Maybe T.Text
            , chat_LastName :: Maybe T.Text
            , chat_UserName :: Maybe T.Text
            } deriving Show         
instance FromJSON Chat where
    parseJSON (Object v) = 
           Chat <$> v .: "id"
                <*> v .: "type"
                <*> v .:? "title"
                <*> v .:? "username"
                <*> v .:? "first_name"
                <*> v .:? "last_name"  

data InlineQuery = InlineQuery
                   { idIQ :: T.Text
                   , fromIQ :: User
                   , queryIQ :: T.Text
                   , offsetIQ :: T.Text
                   } deriving Show
instance FromJSON InlineQuery where
    parseJSON (Object v) = 
        InlineQuery <$> v .: "id"
                    <*> v .: "from"
                    <*> v .: "query"
                    <*> v .: "offset"                                              
data MessageEntity = MessageEntity
                     { entityType :: T.Text
                     , offset :: Int
                     , entityLength :: Int
                     , url :: Maybe T.Text
                     , userME :: Maybe User
                     , languageME :: Maybe T.Text
                     } deriving Show
instance FromJSON MessageEntity  where
    parseJSON (Object v) = 
        MessageEntity   <$> v .: "type"
                        <*> v .: "offset"
                        <*> v .: "length"
                        <*> v .:? "url"
                        <*> v .:? "user"
                        <*> v .:? "language"                      
data PhotoSize = PhotoSize 
            { file_id :: T.Text
            , file_unique_id :: T.Text
            , file_size :: Maybe Int
            , width :: Int
            , height :: Int
            } deriving (Show, Generic)
instance FromJSON PhotoSize         
data User = User 
            { id :: Int
            , is_bot :: Bool
            , first_name :: T.Text
            , last_name :: Maybe T.Text
            , username :: Maybe T.Text
            , language_code :: Maybe T.Text
            } deriving (Show, Generic)
instance FromJSON User             
data Animation = Animation
                 { file_idAn :: T.Text
                 , file_unique_idAn :: T.Text
                 , widthAn :: Int
                 , heightAn :: Int
                 , durationAn :: Int
                 , thumb :: Maybe PhotoSize
                 , file_nameAn :: Maybe T.Text
                 , mime_type :: Maybe T.Text
                 , file_sizeAn :: Maybe Int
                 } deriving Show
instance FromJSON Animation where
    parseJSON (Object v) = 
        Animation   <$> v .: "file_id"
                    <*> v .: "file_unique_id"
                    <*> v .: "width"
                    <*> v .: "height"
                    <*> v .: "duration"
                    <*> v .:? "thumb"
                    <*> v .:? "file_name"
                    <*> v .:? "mime_type"
                    <*> v .:? "file_size" 
                             
data Audio = Audio 
            { file_idAud :: T.Text
            , file_unique_idAud :: T.Text
            , durationAud :: Int
            } deriving Show
instance FromJSON Audio where
    parseJSON (Object v) = 
        Audio   <$> v .: "file_id"
                <*> v .: "file_unique_id"
                <*> v .: "duration"
data Document = Document
                { file_idDoc :: T.Text
                , file_unique_idDoc :: T.Text
                } deriving Show
instance FromJSON Document where
    parseJSON (Object v) = 
        Document <$> v .: "file_id"
                 <*> v .: "file_unique_id"
data Video = Video
             { file_idVid :: T.Text
             , file_unique_idVid :: T.Text
             , widthVid :: Int
             , heightVid :: Int
             , durationVid :: Int
             } deriving Show
instance FromJSON Video where
    parseJSON (Object v) = 
        Video   <$> v .: "file_id"
                <*> v .: "file_unique_id"
                <*> v .: "width"
                <*> v .: "height"
                <*> v .: "duration" 
data Voice = Voice
             { file_idV :: T.Text
             , file_unique_idV :: T.Text
             , durationV :: Int
             } deriving Show
instance FromJSON Voice where
    parseJSON (Object v) = 
        Voice   <$> v .: "file_id"
                <*> v .: "file_unique_id"
                <*> v .: "duration"
data Sticker = Sticker
               { file_idSt :: T.Text
               , file_unique_idSt :: T.Text
               , widthSt :: Int
               , heightSt :: Int
               , is_animated :: Bool
               } deriving Show
instance FromJSON Sticker where
    parseJSON (Object v) = 
        Sticker <$> v .: "file_id"
                <*> v .: "file_unique_id"
                <*> v .: "width"
                <*> v .: "height"
                <*> v .: "is_animated"
data Contact = Contact
               { phone_number :: T.Text
               , first_nameCon :: T.Text
               , last_nameCon :: Maybe T.Text
               , user_idCon :: Maybe Int
               , vcard :: Maybe T.Text
               } deriving Show
instance FromJSON Contact where
    parseJSON (Object v) =
        Contact <$> v .: "phone_number" 
                <*> v .: "first_name"
                <*> v .:? "last_name"
                <*> v .:? "user_id"
                <*> v .:? "vcard"
data VideoNote = VideoNote
                 { file_idVN :: T.Text
                 , file_unique_idVN :: T.Text
                 , lengthVN :: Maybe Int
                 , durationVN :: Maybe Int
                 , thumbVN :: Maybe PhotoSize
                 , file_sizeVN :: Maybe Int
                 } deriving Show
instance FromJSON VideoNote where
    parseJSON (Object v) = 
        VideoNote <$> v .: "file_id"
                  <*> v .: "file_unique_id"
                  <*> v .: "length"
                  <*> v .: "duration" 
                  <*> v .: "thumb"
                  <*> v .: "file_size"                                  
{- type ChatID = BC.ByteString
type ReseivedMessage = BC.ByteString
type SendingMethod = BC.ByteString
type PrefixMessage = BC.ByteString

getMessageChatID :: Maybe Message -> ChatID
getMessageChatID maybeMessage = case maybeMessage of
    Just message -> BC.pack . show . chat_id . chat $ message
    Nothing      -> error "message don't reseived"

getMessageContent :: Maybe Message -> ReseivedMessage   
getMessageContent maybeMessage = case maybeMessage of 
    Just message -> parseMessageContent message
    Nothing      -> error "message don't reseived"  
    where parseMessageContent input
           | isJust $ text input  = encodeUtf8 . fromJust $ text input 
           | isJust $ animation input  = encodeUtf8 . file_idAn . fromJust $ animation input
           | isJust $ audio input  = encodeUtf8 . file_idAud . fromJust $ audio input    
           | isJust $ photo input  = encodeUtf8 .file_id . head . fromJust $ photo input 
           | isJust $ document input  = encodeUtf8 . file_idDoc . fromJust $ document input 
           | isJust $ video input  = encodeUtf8 . file_idVid . fromJust $ video input             
           | isJust $ voice input  = encodeUtf8 . file_idV . fromJust $ voice input         
           | isJust $ sticker input  = encodeUtf8 . file_idSt . fromJust $ sticker input                                                                               
           | isJust $ contact input  = encodeUtf8 $ (phone_number . fromJust $ contact input) 
                                       <> "&first_name=" <> (first_nameCon . fromJust $ contact input)
           | isJust $ video_note input = encodeUtf8 . file_idVN . fromJust $ video_note input
           | otherwise = "Can't return your message yet!"
                                   
getSendingMethod :: Maybe Message -> SendingMethod
getSendingMethod maybeMessage = case maybeMessage of 
    Just message -> parseMessageContent message
    Nothing      -> error "message don't reseived"  
    where parseMessageContent input
           | isJust $ animation input  = "/sendAnimation"
           | isJust $ audio input  = "/sendAudio"    
           | isJust $ photo input  = "/sendPhoto" 
           | isJust $ document input  = "/sendDocument" 
           | isJust $ video input  = "/sendVideo"   
           | isJust $ video_note input = "/sendVideoNote"          
           | isJust $ voice input  = "/sendVoice"        
           | isJust $ sticker input  = "/sendSticker"             
           | isJust $ contact input  = "/sendContact" 
           | otherwise = "/sendMessage"

getPrefix :: Maybe Message -> PrefixMessage
getPrefix maybeMessage = case maybeMessage of 
    Just message -> parseMessageContent message
    Nothing      -> error "message don't reseived"  
    where parseMessageContent input
           | isJust $ text input  = "&text="
           | isJust $ sticker input = "&sticker="
           | isJust $ photo input = "&photo="
           | isJust $ voice input = "&voice="
           | isJust $ contact input = "&phone_number="
           | isJust $ animation input = "&animation="
           | isJust $ audio input = "&audio="
           | isJust $ video  input = "&video="
           | isJust $ video_note input = "&video_note="
           | isJust $ document input = "&document="
 -}
