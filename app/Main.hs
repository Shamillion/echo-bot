{-# LANGUAGE OverloadedStrings, DeriveGeneric #-} 
module Main where

import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Identity
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple
import Data.Aeson
import Data.Text as T hiding (last) 
import GHC.Generics


data Chat = Chat { id :: Int } deriving (Show, Generic)              

data Message = Message {
                         message_id :: Int                  --    
                       , chat :: Chat
                       } deriving (Show, Generic)             

data MessageDate = MessageDate {
                                 update_id :: Int
                               , message :: Message                      
                               } deriving (Show, Generic)

data WholeObject = WholeObject {
                                  ok :: Bool
                               ,  result :: [MessageDate] 
                               } deriving (Show, Generic)

instance FromJSON Chat 
instance FromJSON Message                                
instance FromJSON MessageDate                               
instance FromJSON WholeObject                               



objectFromJSON :: LC.ByteString -> Maybe WholeObject
objectFromJSON a = decode a

myToken :: String
myToken = Prelude.init $ unsafePerformIO $ getToken "../token.tg"

getToken :: String -> IO String
getToken fileName = readFile fileName

messengerHost :: String
messengerHost = "api.telegram.org/bot"

getUpdates :: String
getUpdates = "/getUpdates"

stringRequest :: String -> Request
stringRequest str = parseRequest_ $ 
  mconcat ["https://", messengerHost, myToken, str]

sendRequest :: IO LC.ByteString
sendRequest = do
    res <- httpLBS $ stringRequest getUpdates
    if (getResponseStatusCode res == 200 )
      then pure $ getResponseBody res
      else pure "Error! Broken request!"
    
stringRepeat :: Maybe WholeObject -> String
stringRepeat obj  = runIdentity $ do
  let messageId = show $ message_id message' 
  let chatId    = show $ Main.id $ chat message'
  pure $ mconcat [
                   "/copyMessage?chat_id=", chatId, 
                   "&from_chat_id=",        chatId,
                   "&message_id=",          messageId  
                 ] 
  where  
    Just message' = message <$> last <$> result <$> obj                       

sandRepeats :: Int -> String -> IO (Response LC.ByteString)
sandRepeats n str  
  | n <= 1 = repeatRequest
  | otherwise = do
                  repeatRequest
                  sandRepeats (n-1) str 
  where
    repeatRequest = httpLBS $ stringRequest $ str                    
                            
                                                  
    


main :: IO ()
main = do
    x <- sendRequest
    let obj = objectFromJSON x
    print $ last <$> result <$> obj
    sandRepeats 1 $ stringRepeat obj
    print "Ok"
    



