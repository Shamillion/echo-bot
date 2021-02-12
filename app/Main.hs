{-# LANGUAGE OverloadedStrings, DeriveGeneric #-} 
module Main where

import System.IO
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple
import Data.Aeson
import Data.Text as T
import GHC.Generics

data Chat = Chat {
                   id :: Int
                 , first_name :: T.Text
                 , username :: T.Text 
                 , typeChat :: T.Text
                 } deriving Show

instance FromJSON Chat where
  parseJSON (Object v) =
    Chat <$> v .: "id" 
         <*> v .: "first_name"
         <*> v .: "username"
         <*> v .: "type"
 
data From = From {
                   from_id :: Int
                 , is_bot :: Bool
                 , from_first_name :: T.Text
                 , from_username :: T.Text 
                 , language_code :: T.Text
                 } deriving Show

instance FromJSON From where
  parseJSON (Object v) =
    From <$> v .: "id"
         <*> v .: "is_bot" 
         <*> v .: "first_name"
         <*> v .: "username"
         <*> v .: "language_code"                 

data Message = Message {
                         message_id :: Int
                       , from :: From
                       , chat :: Chat
                       , date :: Int 
                       , text :: T.Text
                       } deriving (Show, Generic)
instance FromJSON Message                          

data Pastoral = Pastoral {
                           update_id :: Int
                         , message :: Message                      
                         } deriving (Show, Generic)
instance FromJSON Pastoral

data WholeObject = WholeObject {
                                  ok :: Bool
                               ,  result :: [Pastoral] 
                               } deriving (Show, Generic)
instance FromJSON WholeObject                               

objectFromJSON :: LC.ByteString -> Maybe WholeObject
objectFromJSON a = decode a

myToken :: String
myToken = Prelude.init $ unsafePerformIO $ getToken "../token.tg"

getToken :: String -> IO String
getToken fileName = readFile fileName

messengerHost :: String
messengerHost = "api.telegram.org/bot"

apiMethod :: String
apiMethod = "/getUpdates"

stringRequest :: Request
stringRequest = parseRequest_ $ 
  mconcat ["https://", messengerHost, myToken, apiMethod]

sendRequest :: IO LC.ByteString
sendRequest = do
    res <- httpLBS $ stringRequest 
    if (getResponseStatusCode res == 200 )
      then pure $ getResponseBody res
      else pure "Error! Broken request!"
    
    


main :: IO ()
main = do
    x <- sendRequest
    print $ objectFromJSON x



