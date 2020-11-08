{-# LANGUAGE OverloadedStrings #-}
module Users where

import Prelude hiding (id)
import TelegramAPI (id, from, message, result, TelegramResponse)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BC

getUserID :: TelegramResponse -> Maybe Int
getUserID decodeUpdate =  
            if null (result decodeUpdate) 
            then Nothing
            else (<$>) id ((message . head . result $ decodeUpdate) >>= from)   

-- потом доделать для случая, когда 1 аргумент нофинг
checkUser :: Maybe Int -> Map.Map Int Int -> Bool
checkUser maybeUserID userMap = let maybeValue = maybeUserID >>= (`Map.lookup` userMap) 
                                in case maybeValue of
                                        Nothing -> False
                                        Just _  -> True

addUserToMap :: Int -> Map.Map Int Int -> Map.Map Int Int
addUserToMap userID userMap = if checkUser (return userID) userMap
                              then userMap
                              else Map.insert userID 2 userMap 

intToBS :: Int -> BC.ByteString
intToBS = BC.pack . show

bsToInt :: BC.ByteString -> Int 
bsToInt = read . BC.unpack 

writeMapToFile :: FilePath -> Map.Map Int Int -> IO ()
writeMapToFile filePath map = BC.writeFile filePath usersBS where
    usersBS = foldr (\(a, b) str -> str <> intToBS a <> " " <> intToBS b <> "\n")
              "" $ Map.toList map

readMapFromFile :: FilePath -> IO (Map.Map Int Int)
readMapFromFile filePath = do 
    fileUsers <- BC.readFile filePath
    let content = map BC.words $ BC.lines $ fileUsers
    let list = listToPairs content
    return $ Map.fromList list


listToPairs :: (Read a, Read b) => [[BC.ByteString]] -> [(a, b)]
listToPairs ass = map (\xs -> (read . BC.unpack . head $ xs, read . BC.unpack . last $ xs)) ass 

f :: Map.Map Int Int
f = Map.fromList [(12,3),(424,6),(2,1)]

g :: Map.Map Int Int
g = Map.empty
