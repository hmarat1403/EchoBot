{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser (someFunc)
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BC 
import Data.Aeson.Lens
import Control.Lens


main :: IO ()
main = do
    print "It's a Bot"
