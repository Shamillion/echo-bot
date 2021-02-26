{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Lazy as Map
import Control.Monad
import Control.Monad.Identity (Identity (runIdentity))
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Text as T hiding (last)
import GHC.Generics (Generic)
import Network.HTTP.Simple
import System.IO
import System.IO.Unsafe (unsafePerformIO)

data Chat = Chat 
  { id :: Int
  , username :: T.Text  
  } deriving (Show, Generic)

data Message = Message
  { message_id :: Int
  , chat :: Chat
  , textM :: Maybe T.Text
  }
  deriving (Show)

data MessageDate = MessageDate
  { update_id :: Int
  , message :: Message
  }
  deriving (Show, Generic)

data WholeObject = WholeObject
  { ok :: Bool
  , result :: [MessageDate]
  }
  deriving (Show, Generic)

newtype KeyboardButton = KeyboardButton
  {text :: T.Text}
  deriving (Show, Generic)

newtype KeyboardButtons = KeyboardButtons [KeyboardButton]
  deriving (Show, Generic)

data ReplyKeyboardMarkup = ReplyKeyboardMarkup
  { keyboard :: [KeyboardButtons]
  , resize_keyboard :: Bool
  , one_time_keyboard :: Bool
  }
  deriving (Show, Generic)

instance FromJSON Chat

instance FromJSON Message where
  parseJSON (Object v) = do
    message_id <- v .: "message_id"
    chat  <- v .: "chat"
    textM <- v .:? "text"
    pure $ Message message_id chat textM

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

type UpdateID = Int

getUpdates :: Int -> String
getUpdates num = mconcat ["/getUpdates?offset=", show num]

lastUpdateID :: UpdateID
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

sandRepeats :: Maybe WholeObject -> 
               (UpdateID, Map.Map Username NumRepeats) -> IO ()
sandRepeats obj env = 
  replicateM_ num $ httpLBS $ stringRequest $ runIdentity $ do
      let messageId = show $ message_id message'
      let chatId = show $ Main.id $ chat message'      
      pure $
        mconcat
          [ "/copyMessage?chat_id="
          , chatId
          , "&from_chat_id="
          , chatId
          , "&message_id="
          , messageId
          ]
     where
      Just message' = message <$> last <$> result <$> obj
      usrName = username $ chat message'
      num = case (Map.lookup usrName $ snd env) of
              Nothing -> 3
              Just n  -> n

getUpdateID :: Maybe WholeObject -> UpdateID
getUpdateID obj = val
 where
  Just val = update_id <$> last <$> result <$> obj

ifKeyWord :: Maybe WholeObject -> IO (Response LC.ByteString)
ifKeyWord obj = case textM message' of
  Just "/repeat" -> sendKeyboard obj
  Just "/help" -> httpLBS $ stringRequest "Help will come to you soon!"
  _ -> httpLBS $ stringRequest "zzz"
 where
  Just message' = message <$> last <$> result <$> obj

one :: KeyboardButton
one =
  KeyboardButton
    { text = "1"
    }

two :: KeyboardButton
two =
  KeyboardButton
    { text = "2"
    }

three :: KeyboardButton
three =
  KeyboardButton
    { text = "3"
    }

four :: KeyboardButton
four =
  KeyboardButton
    { text = "4"
    }

five :: KeyboardButton
five =
  KeyboardButton
    { text = "5"
    }

buttons :: KeyboardButtons
buttons = KeyboardButtons [one, two, three, four, five]

numRepeat :: ReplyKeyboardMarkup
numRepeat =
  ReplyKeyboardMarkup
    { keyboard = [buttons]
    , resize_keyboard = True
    , one_time_keyboard = True
    }

question :: String
question = "Currently set to 4 repetitions.\nHow many reps do you want to set?"

stringToUrl :: String -> String
stringToUrl str = Prelude.foldl fun "" str
 where
  fun acc c =
    acc ++ case c of
      '{' -> "%7B"
      '}' -> "%7D"
      '[' -> "%5B"
      ']' -> "%5D"
      '\"' -> "%22"
      '\n' -> "%0A"
      ' ' -> "%20"
      ',' -> "%2C"
      '.' -> "%2E"
      ':' -> "%3A"
      _ -> [c]

sendKeyboard :: Maybe WholeObject -> IO (Response LC.ByteString)
sendKeyboard obj =
  httpLBS $  
    stringRequest $    
      mconcat
        [ "/sendMessage?chat_id="
        , show $ Main.id $ chat message'
        , "&text="
        , stringToUrl question
        , "&reply_markup="
        , stringToUrl $ LC.unpack $ encode numRepeat
        ]
 where
  Just message' = message <$> last <$> result <$> obj
  
type Username   = T.Text
type NumRepeats = Int
  
environment :: (UpdateID, Map.Map Username NumRepeats) 
environment = (0, Map.singleton "" 3) 

endlessCycle :: (UpdateID, Map.Map Username NumRepeats) -> IO ()
endlessCycle env = do
  x <- httpLBS $ stringRequest $ getUpdates $ fst env
  case getResponseStatusCode x == 200 of
    False -> print "Error! Broken request!"
    _ -> do
      let obj = objectFromJSON $ getResponseBody x
      case result <$> obj of
        Just [] -> endlessCycle env
        _ -> do
          let newEnv = (1 + getUpdateID obj, snd env)
          ifKeyWord obj
          print $ last <$> result <$> obj
          sandRepeats obj newEnv
         -- sendKeyboard obj
          endlessCycle newEnv
          

main :: IO ()
main = do
  endlessCycle environment

