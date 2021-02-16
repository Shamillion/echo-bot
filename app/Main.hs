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

--getToken :: String -> IO String
--getToken = readFile 

messengerHost :: String
messengerHost = "api.telegram.org/bot"

getUpdates :: String
getUpdates = "/getUpdates"

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
    

main :: IO ()
main = do
  x <- httpLBS $ stringRequest getUpdates -- sendRequest
  let obj = objectFromJSON $ getResponseBody x
  case result <$> obj of
    Just [] -> main
    _       -> do
      if getUpdateID obj == lastUpdateID
        then main
        else do  
            print $ result <$> obj
            sandRepeats 3 $ stringRepeat obj
            print "Ok"
   --print $ myToken
   --print $ lastUpdateID             
             
    
