{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Identity ( Identity(runIdentity) )
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Text as T hiding (last)
import GHC.Generics ( Generic )
import Network.HTTP.Simple
import System.IO
import System.IO.Unsafe (unsafePerformIO)

newtype Chat = Chat {id :: Int} deriving (Show, Generic)

data Message = Message
  { message_id :: Int, 
    chat :: Chat
  }
  deriving (Show, Generic)

data MessageDate = MessageDate
  { update_id :: Int,
    message :: Message
  }
  deriving (Show, Generic)

data WholeObject = WholeObject
  { ok :: Bool,
    result :: [MessageDate]
  }
  deriving (Show, Generic)

instance FromJSON Chat

instance FromJSON Message

instance FromJSON MessageDate

instance FromJSON WholeObject

objectFromJSON :: LC.ByteString -> Maybe WholeObject
objectFromJSON = decode 

myToken :: String
myToken = getDataFromFile "../config.cfg" 1

messengerHost :: String
messengerHost = "api.telegram.org/bot"

getUpdates :: Int -> String
getUpdates num = mconcat ["/getUpdates?offset=", show num]

lastUpdateID = read $ getDataFromFile "../config.cfg" 2 :: Int

getDataFromFile :: String -> Int -> String
getDataFromFile fileName num =  
  unsafePerformIO $ do
    file <- openFile fileName ReadMode
    firstLine <- hGetLine file    
    secondLine <- hGetLine file    
    hClose file
    case num of
     1 -> pure firstLine
     _ -> pure secondLine
    
  

stringRequest :: String -> Request
stringRequest str =
  parseRequest_ $
    mconcat ["https://", messengerHost, myToken, str]

--sendRequest :: IO LC.ByteString
--sendRequest = do
  --res <- httpLBS $ stringRequest getUpdates
  --if getResponseStatusCode res == 200
    --then pure $ getResponseBody res
    --else pure "Error! Broken request!"

stringRepeat :: Maybe WholeObject -> String
stringRepeat obj = runIdentity $ do
  let messageId = show $ message_id message'
  let chatId = show $ Main.id $ chat message'
  pure $
    mconcat
      [ "/copyMessage?chat_id=",
        chatId,
        "&from_chat_id=",
        chatId,
        "&message_id=",
        messageId
      ]
  where
    Just message' = message <$> last <$> result <$> obj

sandRepeats :: Int -> String -> IO (Response LC.ByteString)
sandRepeats n str
  | n <= 1 = repeatRequest
  | otherwise = do
    repeatRequest
    sandRepeats (n -1) str
  where
    repeatRequest = httpLBS $ stringRequest $ str
    
getUpdateID :: Maybe WholeObject -> Int 
getUpdateID obj = val 
  where Just val = update_id <$> last <$> result <$> obj   
            
endlessCycle :: Int -> IO ()
endlessCycle updateID = do
  x <- httpLBS $ stringRequest $ getUpdates updateID 
  case getResponseStatusCode x == 200 of 
    False -> print "Error! Broken request!"
    _     -> do
      let obj = objectFromJSON $ getResponseBody x
      case result <$> obj of
        Just [] -> endlessCycle updateID
        _       -> do
          let newUpdateID = 1 + getUpdateID obj
          print $ last <$> result <$> obj
          sandRepeats 3 $ stringRepeat obj
          endlessCycle newUpdateID           
                

           
             
main :: IO ()
main = do 
  endlessCycle 0 
  --httpLBS $ stringRequest "/sendMessage?chat_id=195352543&text=Currently&reply_markup=%7B%22keyboard%22+%3A+%5B%5B%7B%22text%22%3A%221%22%7D%2C%7B%22text%22%3A%222%22%7D%2C+%7B%22text%22%3A%223%22%7D%2C+%7B%22text%22%3A%224%22%7D%2C+%7B%22text%22%3A%225%22%7D%5D%5D%2C+%22resize_keyboard%22+%3A+true%2C+%22one_time_keyboard%22+%3A+true%7D"
  --print "OK"
