{-# LANGUAGE OverloadedStrings #-}
module Parser
    ( someFunc
    ) where

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BC 
--import Data.Aeson.Lens
import Control.Lens

someFunc :: IO ()
someFunc = putStrLn "someFunc"
