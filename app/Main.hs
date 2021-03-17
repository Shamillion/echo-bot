{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Lazy as Map
import Control.Monad
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State.Lazy
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
    
message' :: MessageDate -> Message    
message' obj = message $ obj

sandRepeats :: MessageDate -> Environment -> IO ()
sandRepeats obj env = 
  replicateM_ num $ httpLBS $ stringRequest $ runIdentity $ do
      let messageId = show $ message_id $ message' obj
      let chatId = show $ Main.id $ chat $ message' obj      
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
       num = getRepeats obj env

ifKeyWord :: MessageDate -> StateT Environment IO ()
ifKeyWord obj = do
  env <- get
  let Just arr = result <$> evalState getData env
      newObj = Prelude.head arr
      newEnv = Environment (1 + update_id newObj) (userData env)
      Just val = textM $ message' newObj
      usrName = username $ chat $ message' newObj
  case (textM $ message' obj) of
    Just "/repeat" -> do    
      lift $ sendKeyboard obj env
      wordIsRepeat obj arr 
        
    Just "/help" -> do
      lift $ sendComment obj $ "Help will come to you soon!"
      lift $ print "Log: The help sent"
      pure ()
      
    _ -> do
      lift $ sandRepeats obj env
      pure () 
  
  
wordIsRepeat :: MessageDate -> [MessageDate] -> StateT Environment IO () 
wordIsRepeat obj [] = do
  env <- get
  let Just newArr = result <$> evalState getData env
  wordIsRepeat obj newArr
  
wordIsRepeat obj (x:xs) = do
  env <- get
  let newObj = x
      newEnv = Environment (1 + update_id newObj) (userData env)
      Just val = textM $ message' newObj
      usrName = username $ chat $ message' obj
      newUsrName = username $ chat $ message' newObj 
  case (usrName == newUsrName) of
    True -> 
      case (elem val ["1","2","3","4","5"]) of      
        True -> do        
          lift $ sendComment obj $  "Done! Set up " 
                                 ++ T.unpack val 
                                 ++ " repeat(s)."
          lift $ print $  "Log: Set up " 
                       ++ T.unpack val 
                       ++ " repeat(s)."
          put  $ Environment (1 + update_id newObj) 
                             (Map.insert usrName (read $ T.unpack val) 
                                                        (userData env))                                                                 
        _ -> do                     
          lift $ sandRepeats newObj newEnv
          put newEnv 
                     
    _ -> do
      ifKeyWord newObj
      put newEnv 
      wordIsRepeat obj xs
                     

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
buttons =  KeyboardButtons [one, two, three, four, five]

numRepeat :: ReplyKeyboardMarkup
numRepeat =
  ReplyKeyboardMarkup
    { keyboard = [buttons]
    , resize_keyboard = True
    , one_time_keyboard = True
    }

question :: MessageDate ->  Environment -> String
question obj env = 
  mconcat 
    [ "Currently set to "
    , show $ getRepeats obj env
    , " repetitions.\nHow many reps do you want to set?"
    ]

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

sendKeyboard :: MessageDate -> Environment -> IO (Response LC.ByteString)
sendKeyboard obj env =
  httpLBS $  
    stringRequest $    
      mconcat
        [ "/sendMessage?chat_id="
        , show $ Main.id $ chat $ message' obj
        , "&text="
        , stringToUrl $ question obj env
        , "&reply_markup="
        , stringToUrl $ LC.unpack $ encode numRepeat
        ]
        
sendComment :: MessageDate -> String -> IO (Response LC.ByteString)
sendComment obj str =
  httpLBS $  
    stringRequest $    
      mconcat
        [ "/sendMessage?chat_id="
        , show $ Main.id $ chat $ message' obj
        , "&text="
        , stringToUrl $ str
        ]        
  
type Username   = T.Text
type NumRepeats = Int
  
data Environment = Environment 
  { lastUpdate  :: UpdateID
  , userData    :: Map.Map Username NumRepeats
  }

getRepeats :: MessageDate ->  Environment -> Int
getRepeats obj env = case (Map.lookup usrName $ userData env) of
            Nothing -> 3
            Just n  -> n
  where      
    usrName = username $ chat $ message' obj     

environment :: Environment  
environment =  Environment 0 $ Map.singleton "" 3 

getData :: State Environment (Maybe WholeObject)
getData =  do
  env <- get
  let x = unsafePerformIO $ httpLBS $ stringRequest $ getUpdates $ lastUpdate env
  case getResponseStatusCode x == 200 of
    False -> pure Nothing
    _ -> do
      let obj = objectFromJSON $ (getResponseBody x)
      case result <$> obj of
        Just [] -> do
          put $ Environment 1 (userData env)
          getData 
        _ -> pure obj                

firstUpdateIDSession :: StateT Environment IO ()
firstUpdateIDSession =  do
  env <- get
  let (obj, newEnv) = runState getData env                
  case obj of
    Nothing -> pure ()              
    _ -> do
      let Just arr = result <$> obj 
      if lastUpdate newEnv == 1
      then put $ Environment (update_id $ last arr) (userData newEnv) 
      else put $ Environment (1 + (update_id $ last arr)) (userData newEnv)           

endlessCycle :: StateT Environment IO ()
endlessCycle =  do
  env <- get
  let obj = evalState getData env          
  case obj of
    Nothing -> lift $ print "Error! Broken request!"               
    _ -> do
          let Just arr = result <$> obj
          put $ Environment (1 + (update_id $ last arr)) (userData env)          
          newEnv <- get
          mapM_ ifKeyWord arr   
          lift $ print $ arr                              
          endlessCycle 
          pure () 
          

main :: IO ()
main = do
  newEnv <- execStateT firstUpdateIDSession environment
  evalStateT endlessCycle newEnv
 

