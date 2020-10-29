{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module JsonData where

import qualified Data.Text as T
import GHC.Generics
import Data.Aeson
import Control.Monad

data TelegramResponse = Response 
                        { ok :: Bool
                        , result :: [Results]
                        } deriving (Show, Generic)
instance FromJSON TelegramResponse                        
data Results = Results
              { update_id :: Int
              , message :: Message 
              } deriving Show
instance FromJSON Results where
    parseJSON (Object v) = 
        Results <$> v .: "update_id"
                <*> v .: "message"                          
data Message = Message
               { message_id :: Int
               , from :: From
               , chat :: Chat
               , date :: Int
               , text :: T.Text
               } deriving (Show, Generic)
instance FromJSON Message               
data From = From 
            { id :: Int
            , is_bot :: Bool
            , first_name :: T.Text
            , last_name :: T.Text
            , username :: T.Text
            , language_code :: T.Text
            } deriving (Show, Generic)
instance FromJSON From             
{-instance FromJSON From where
    parseJSON (Object v) = 
           From <$> v .: "id"
                <*> v .: "is_bot"
                <*> v .: "user"
                <*> v .: "language_code" 
-}                                      
data Chat = Chat
            { chat_id :: Int
            , chat_FirstName :: T.Text
            , chat_LastName :: T.Text
            , chat_UserName :: T.Text
            , chat_Type :: T.Text
            } deriving Show         
instance FromJSON Chat where
    parseJSON (Object v) = 
           Chat <$> v .: "id"
                <*> v .: "first_name"
                <*> v .: "last_name"
                <*> v .: "username"
                <*> v .: "type"                
{-data User = User 
            { first_name :: T.Text
            , last_name :: T.Text
            , userName :: T.Text
            } deriving (Show, Generic)
instance FromJSON User            
-}
printResults :: Maybe [Results] -> IO ()
printResults Nothing = print "error loading data"
printResults (Just results) = do
    forM_ results (print . text . message)
