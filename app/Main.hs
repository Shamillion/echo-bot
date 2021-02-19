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

newtype KeyboardButton = KeyboardButton
  { text :: T.Text} deriving (Show, Generic) 
  
newtype KeyboardButtons = KeyboardButtons [KeyboardButton]  
  deriving (Show, Generic) 
  
data ReplyKeyboardMarkup = ReplyKeyboardMarkup
  { keyboard :: [KeyboardButtons],
    resize_keyboard :: Bool,
    one_time_keyboard :: Bool
  }
  deriving (Show, Generic)  

instance FromJSON Chat

instance FromJSON Message

instance FromJSON MessageDate

instance FromJSON WholeObject

instance ToJSON KeyboardButton

instance ToJSON KeyboardButtons

instance ToJSON ReplyKeyboardMarkup

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

one :: KeyboardButton
one = KeyboardButton 
  {text = "1"}
  
two :: KeyboardButton
two = KeyboardButton 
  {text = "2"}
  
three :: KeyboardButton
three = KeyboardButton 
  {text = "3"}
  
four :: KeyboardButton
four = KeyboardButton 
  {text = "4"}        

five :: KeyboardButton
five = KeyboardButton 
  {text = "5"}

buttons :: KeyboardButtons
buttons = KeyboardButtons [one, two, three, four, five]

numRepeat :: ReplyKeyboardMarkup
numRepeat = ReplyKeyboardMarkup
  { keyboard = [buttons],
    resize_keyboard = True,
    one_time_keyboard = True
  }
  
sendKeybord :: String -> String
sendKeybord str = Prelude.foldl fun "" str
  where
    fun acc c = 
      acc ++ case c of
                '{' -> "%7B"
                '}' -> "%7D"
                '[' -> "%5B" 
                ']' -> "%5D"
                '\"'-> "%22"
                ' ' -> "+"
                ',' -> "%2C"
                ':' -> "%3A"
                _   -> [c]
                
    
            
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
  --endlessCycle 0 
  httpLBS $ stringRequest $ "/sendMessage?chat_id=195352543&text=Currently%20set%20to%204%20repetitions&reply_markup=" ++ (sendKeybord $ LC.unpack $ encode numRepeat)
  print "OK"

