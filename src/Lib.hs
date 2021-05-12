{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State.Lazy
import Data.Aeson
--import qualified Data.ByteString as B
--import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T hiding (last)
import Data.Time
import GHC.Generics (Generic)
import Network.HTTP.Simple
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Random


data Configuration = Configuration
  { messenger :: T.Text
  , hostTG :: T.Text
  , hostVK :: T.Text
  , tokenTG :: T.Text
  , tokenVK :: T.Text
  , groupIdVK :: Int
  , apiVKVersion :: T.Text
  , helpMess :: [T.Text]
  , repeatMess :: T.Text
  , defaultRepaets :: Int 
  , priorityLevel :: Priority  
  , logOutput :: T.Text
  } deriving (Show, Generic)
  
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
  
data Priority = DEBUG | INFO | WARNING | ERROR  
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Configuration

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

instance FromJSON Priority

time :: IO String
time = (take 19) <$> show <$> getCurrentTime

file :: Handle
file  = unsafePerformIO $ openFile "../log.log" AppendMode

writingLine :: Priority -> String -> IO () 
writingLine lvl str = if (lvl >= logLevel)
    then do
      t <- time
      let string = t ++ " UTC   " ++ fun lvl ++ " - " ++ str   
      case out of
        "file" -> do
          hPutStrLn file string 
          hFlush file 
        _ -> print string   
    else pure ()  
  where
    out = logOutput configuration 
    fun val = case val of
      DEBUG   -> "DEBUG  "
      INFO    -> "INFO   "
      WARNING -> "WARNING"
      ERROR   -> "ERROR  "       

getConfiguration :: String -> Either String Configuration
getConfiguration fileName = 
  unsafePerformIO $ do
    t <- time
    content <- L.readFile fileName 
    let obj = eitherDecode content
    case obj of
      Right _ -> pure obj
      Left e  -> do
        let str = t ++ " UTC   " ++ "ERROR  " ++ " - " ++ e 
        print str
        hPutStrLn file str 
        hFlush file       
        pure obj
 
errorConfig :: Configuration
errorConfig =  Configuration
  { messenger = "Error"
  , hostTG = "Error"
  , hostVK = "Error"
  , tokenTG = "Error"
  , tokenVK = "Error"
  , groupIdVK = 0
  , apiVKVersion = "Error"
  , helpMess = ["Error"]
  , repeatMess = "Error"
  , defaultRepaets = 0
  , priorityLevel = ERROR 
  , logOutput = "cons"   
  } 

configuration :: Configuration
configuration =
  case getConfiguration "../config.json" of
    Right v -> v
    Left e  -> errorConfig

currentMessenger :: T.Text
currentMessenger = messenger configuration

myHost :: String
myHost =  if currentMessenger == "TG"
          then T.unpack $ hostTG configuration
          else T.unpack $ hostVK configuration                                     

myToken :: String
myToken =  if currentMessenger == "TG"
           then T.unpack $ tokenTG configuration
           else T.unpack $ tokenVK configuration  
 
messengerHost :: String
messengerHost = myHost ++ "/bot"

logLevel :: Priority
logLevel =  priorityLevel configuration

type UpdateID = Int

getUpdates :: Int -> String
getUpdates num = mconcat ["/getUpdates?offset=", show num, "&timeout=1"]

stringRequest :: String -> Request
stringRequest str =
  parseRequest_ $
    mconcat ["https://", messengerHost, myToken, str] 
    
message' :: MessageDate -> Message    
message' obj = message $ obj

forwardMessagesVk :: Int -> MessageDate -> Request
forwardMessagesVk randomId' obj = 
  parseRequest_ $  
    mconcat ["https://"
            , myHost
            , "/method/messages.send?user_id="
            , userId
            , "&random_id="
            , show randomId'
            , "&peer_id=-"
            , show $ groupIdVK configuration
            , "&forward_messages="
            , messId 
            , "&access_token="            
            , myToken
            , "&v="
            , T.unpack $ apiVKVersion configuration
            ]
  where
    userId = T.unpack $ username $ chat $ message' obj  
    messId = show $ message_id $ message' obj     

randomId :: IO Int
randomId = randomRIO (1, 1000000)
    
repeatMessageVk :: MessageDate -> IO (Response LC.ByteString)
repeatMessageVk obj = do
  r <- randomId
  let string = forwardMessagesVk r obj 
  writingLine DEBUG $ show string
  httpLBS $ string       

sandRepeats :: MessageDate -> Environment -> IO ()
sandRepeats obj env =     
    replicateM_ num $ 
      if currentMessenger == "TG"
          then do
            writingLine DEBUG $ show string
            httpLBS $ string 
          else repeatMessageVk obj      
  where
    num = getRepeats obj env
    messageId = show $ message_id $ message' obj
    chatId = show $ Lib.id $ chat $ message' obj
    string = stringRequest $
      mconcat
        [ "/copyMessage?chat_id="
        , chatId
        , "&from_chat_id="
        , chatId
        , "&message_id="
        , messageId
        ]   
      
ifKeyWord :: (Int -> Maybe WholeObject) -> MessageDate -> StateT Environment IO ()
ifKeyWord getDataVk obj = do
  env <- get
  let Just arr = result <$> fun 
      fun = if currentMessenger == "TG"
              then evalState getData env
              else getDataVk $ lastUpdate env     
      newObj = Prelude.head arr
      newEnv = Environment (1 + update_id newObj) (userData env)
      Just val = textM $ message' newObj
      usrName = T.unpack $ username $ chat $ message' obj
  case (textM $ message' obj) of
    Just "/repeat" -> do         
      lift $ writingLine INFO $ "Received /repeat from " ++ usrName
      lift $ sendKeyboard obj env       
      wordIsRepeat getDataVk obj arr       
        
    Just "/help" -> do
      lift $ writingLine INFO $ "Received /help from " ++ usrName 
      lift $ sendComment obj $ T.unpack $ mconcat $ helpMess configuration      
      pure ()
      
    _ -> do
      lift $ sandRepeats obj env
      pure () 
  
  
wordIsRepeat :: (Int -> Maybe WholeObject) -> MessageDate -> 
                             [MessageDate] -> StateT Environment IO () 
wordIsRepeat getDataVk obj [] = do
  env <- get
  let Just newArr = result <$> fun 
      fun = if currentMessenger == "TG"
              then evalState getData env
              else getDataVk $ lastUpdate env
  wordIsRepeat getDataVk obj newArr
  
wordIsRepeat getDataVk obj (x:xs) = do
  env <- get
  let newObj = x
      newEnv = Environment (num + update_id newObj) (userData env)
      Just val = textM $ message' newObj
      usrName = username $ chat $ message' obj
      newUsrName = username $ chat $ message' newObj 
      num = if currentMessenger == "TG" then 1 else 0
  case (usrName == newUsrName) of
    True -> 
      case (elem val ["1","2","3","4","5"]) of      
        True -> do        
          lift $ sendComment obj $  "Done! Set up " 
                                 ++ T.unpack val 
                                 ++ " repeat(s)."
          lift $ writingLine INFO $ "Set up " 
                       ++ T.unpack val 
                       ++ " repeat(s) to "
                       ++ T.unpack usrName
          put $ Environment (num + update_id newObj) 
                             (Map.insert usrName (read $ T.unpack val) 
                                                        (userData env))                                                                 
        _ -> do                     
          lift $ sandRepeats newObj newEnv
          put newEnv 
                     
    _ -> do
      ifKeyWord getDataVk newObj
      put newEnv 
      wordIsRepeat getDataVk obj xs
                     

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
    , " repetitions.\n"
    , T.unpack $ repeatMess configuration
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
sendKeyboard obj env = do
    writingLine DEBUG $ show string
    httpLBS string 
  where 
    string = stringRequest $    
      mconcat
        [ "/sendMessage?chat_id="
        , show $ Lib.id $ chat $ message' obj
        , "&text="
        , stringToUrl $ question obj env
        , "&reply_markup="
        , stringToUrl $ LC.unpack $ encode numRepeat
        ]
        
sendComment :: MessageDate -> String -> IO (Response LC.ByteString)
sendComment obj str = do   
    writingLine DEBUG $ show string
    httpLBS string  
  where 
    string = 
      if currentMessenger == "TG"
        then stringRequest $    
               mconcat
                 [ "/sendMessage?chat_id="
                 , show $ Lib.id $ chat $ message' obj
                 , "&text="
                 , stringToUrl str
                 ] 
        else parseRequest_ $  
               mconcat 
                 ["https://"
                 , myHost
                 , "/method/messages.send?user_id="
                 , userId
                 , "&random_id="
                 , randomId' 
                 , "&peer_id=-"
                 , show $ groupIdVK configuration
                 , "&message="
                 , stringToUrl str 
                 , "&access_token="            
                 , myToken
                 , "&v="
                 , T.unpack $ apiVKVersion configuration
                 ]            
    userId = T.unpack $ username $ chat $ message' obj  
    randomId' = show $ message_id $ message' obj      

type Username   = T.Text
type NumRepeats = Int
  
data Environment = Environment 
  { lastUpdate :: UpdateID
  , userData   :: Map.Map Username NumRepeats
  }

getRepeats :: MessageDate ->  Environment -> Int
getRepeats obj env = case (Map.lookup usrName $ userData env) of
            Nothing -> defaultRepaets configuration
            Just n  -> n
  where      
    usrName = username $ chat $ message' obj     

environment :: Environment  
environment =  Environment 0 $ Map.singleton "" (defaultRepaets configuration) 

connection :: Environment -> Int -> IO (Response LC.ByteString)
connection env num = do
    x <- try $ httpLBS string
    writingLine DEBUG $ show string   
    case x of
      Left e -> do
        writingLine ERROR $ show (e :: HttpException)      
        when (num == 0) $ do
          getCurrentTime >>= print 
          print "Connection Failure"
          print "Trying to set a connection... "          
        threadDelay 1000000
        connection env (num + 1)
      Right v -> do 
        writingLine DEBUG $ show v 
        when (num /= 0) $ do         
          getCurrentTime >>= print
          print "Connection restored"
        pure v
  where 
    string = stringRequest $ getUpdates $ lastUpdate env         

getData :: State Environment (Maybe WholeObject)
getData =  do
  env <- get
  let x = unsafePerformIO $ connection env 0
      code = getResponseStatusCode x 
      writing = unsafePerformIO $ writingLine ERROR $ "statusCode" ++ show code
  case code == 200 of
    False -> pure $ unsafePerformIO $ do
      writingLine ERROR $ "statusCode " ++ show code
      pure Nothing
    _ -> do
      let obj = decode $ getResponseBody x
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
      lift $ getCurrentTime >>= print
      lift $ print "Connection established"
      let Just arr = result <$> obj 
      if lastUpdate newEnv == 1
      then put $ Environment (update_id $ last arr) (userData newEnv) 
      else put $ Environment (1 + (update_id $ last arr)) (userData newEnv)           

endlessCycle :: StateT Environment IO ()
endlessCycle =  do
  env <- get  
  let obj = evalState getData env 
      nothing num = Nothing         
  case obj of
    Nothing -> lift $ writingLine ERROR "Broken request!"               
    _ -> do
      let Just arr = result <$> obj
      put $ Environment (1 + (update_id $ last arr)) (userData env)          
      newEnv <- get
      mapM_ (ifKeyWord nothing) arr                                
      endlessCycle   
