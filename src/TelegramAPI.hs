{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}
module TelegramAPI where

import qualified Data.Text as T
import Prelude hiding ( id )
import GHC.Generics (Generic) 
import Data.Aeson
    (genericParseJSON, genericToJSON, FromJSON (parseJSON), ToJSON (toJSON), defaultOptions, Options (..) )

-- telegram responce type
data TelegramResponse = TelegramResponse 
                        { ok :: Bool
                        , result :: [Update]
                        } deriving (Show, Generic)
instance FromJSON TelegramResponse             
instance ToJSON TelegramResponse
-- last 24 hours updates list type
data Update = Update
              { update_id :: Int
              , message :: Maybe Message
              , channel_post :: Maybe Message
              } deriving (Show, Generic)
instance FromJSON Update
instance ToJSON Update
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
               , caption_entities :: Maybe [MessageEntity]
               , contact :: Maybe Contact
               } deriving (Show, Generic)
instance FromJSON Message 
instance ToJSON Message
data Chat = Chat
            { _id :: Int
            , _type :: Maybe T.Text
            , _title :: Maybe T.Text
            , _firstName :: Maybe T.Text
            , _lastName :: Maybe T.Text
            , _userName :: Maybe T.Text
            } deriving (Show, Generic)         
instance FromJSON Chat where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}
instance ToJSON Chat where
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}     
                                     
data MessageEntity = MessageEntity
                     { _type :: T.Text
                     , _offset :: Int
                     , _length :: Int
                     , _url :: Maybe T.Text
                     , _user :: Maybe User
                     , _language :: Maybe T.Text
                     } deriving (Show, Generic)
instance FromJSON MessageEntity where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}
instance ToJSON MessageEntity where
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1} 
                  
data PhotoSize = PhotoSize 
            { file_id :: T.Text
            , file_unique_id :: T.Text
            , file_size :: Maybe Int
            , width :: Int
            , height :: Int
            } deriving (Show, Generic)
instance FromJSON PhotoSize     
instance ToJSON PhotoSize    
data User = User 
            { id :: Int
            , is_bot :: Bool
            , first_name :: T.Text
            , last_name :: Maybe T.Text
            , username :: Maybe T.Text
            , language_code :: Maybe T.Text
            } deriving (Show, Generic)
instance FromJSON User             
instance ToJSON User 
data Animation = Animation
                 { file_id :: T.Text
                 , file_unique_id :: T.Text
                 , width :: Int
                 , height :: Int
                 , duration :: Int
                 , thumb :: Maybe PhotoSize
                 , file_name :: Maybe T.Text
                 , mime_type :: Maybe T.Text
                 , file_size :: Maybe Int
                 } deriving (Show, Generic)
instance FromJSON Animation 
instance ToJSON Animation
                       
data Audio = Audio 
            { file_id :: T.Text
            , file_unique_id :: T.Text
            , duration :: Int
            } deriving (Show, Generic)
instance FromJSON Audio 
instance ToJSON Audio

data Document = Document
                { file_id :: T.Text
                , file_unique_id :: T.Text
                } deriving (Show, Generic)
instance FromJSON Document 
instance ToJSON Document

data Video = Video
             { file_id :: T.Text
             , file_unique_id :: T.Text
             , width :: Int
             , height :: Int
             , duration :: Int
             } deriving (Show, Generic)
instance FromJSON Video 
instance ToJSON Video

data Voice = Voice
             { file_id :: T.Text
             , file_unique_id :: T.Text
             , durationV:: Int
             } deriving (Show, Generic)
instance FromJSON Voice
instance ToJSON Voice

data Sticker = Sticker
               { file_id :: T.Text
               , file_unique_id :: T.Text
               , width :: Int
               , height :: Int
               , is_animated :: Bool
               } deriving (Show, Generic)
instance FromJSON Sticker
instance ToJSON Sticker

data Contact = Contact
               { phone_number :: T.Text
               , first_name :: T.Text
               , last_name :: Maybe T.Text
               , user_id :: Maybe Int
               , vcard :: Maybe T.Text
               } deriving (Show, Generic)
instance FromJSON Contact
instance ToJSON Contact

data VideoNote = VideoNote
                 { file_id :: T.Text
                 , file_unique_id :: T.Text
                 , length :: Maybe Int
                 , duration :: Maybe Int
                 , thumb :: Maybe PhotoSize
                 , file_size :: Maybe Int
                 } deriving (Show, Generic)
instance FromJSON VideoNote
instance ToJSON VideoNote
newtype ReplyKeyboardMarkUp = ReplyKeyboardMarkUp
                           { keyBoard :: [[KeyboardButton]]
                           } deriving (Show, Generic)
instance FromJSON ReplyKeyboardMarkUp
instance ToJSON ReplyKeyboardMarkUp                           
newtype KeyboardButton = KeyboardButton 
                      { text :: T.Text
                      } deriving (Show, Generic)
instance FromJSON KeyboardButton
instance ToJSON KeyboardButton                      
