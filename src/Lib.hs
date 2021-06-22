{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad.State.Lazy
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T hiding (last)
import Data.Time (getCurrentTime)
import GHC.Generics (Generic)
import Network.HTTP.Simple
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Random (Random (randomRIO))

data Configuration = Configuration -- Data type for the configuration file.
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
  }
  deriving (Show, Generic)

instance FromJSON Configuration

data Chat = Chat                   -- Data types for the Telegram answer.
  { id :: Int
  , username :: T.Text
  }
  deriving (Show, Generic)

instance FromJSON Chat

data Message = Message
  { message_id :: Int
  , chat :: Chat
  , textM :: Maybe T.Text
  }
  deriving (Show)

instance FromJSON Message where
  parseJSON (Object v) = do
    message_id <- v .: "message_id"
    chat <- v .: "chat"
    textM <- v .:? "text"
    pure $ Message message_id chat textM

data MessageDate = MessageDate
  { update_id :: Int
  , message :: Message
  }
  deriving (Show, Generic)

instance FromJSON MessageDate

data WholeObject = WholeObject
  { ok :: Bool
  , result :: [MessageDate]
  }
  deriving (Show, Generic)

instance FromJSON WholeObject

newtype KeyboardButton = KeyboardButton -- Data type for the Telegram keyboard.
  {text :: T.Text}
  deriving (Show, Generic)

instance ToJSON KeyboardButton

data ReplyKeyboardMarkup = ReplyKeyboardMarkup
  { keyboard :: [[KeyboardButton]]
  , resize_keyboard :: Bool
  , one_time_keyboard :: Bool
  }
  deriving (Show, Generic)

instance ToJSON ReplyKeyboardMarkup

data Priority = DEBUG | INFO | WARNING | ERROR -- Data type for the logger.
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Priority

data ActionVk = ActionVk -- Data types for the VK keyboard.
  { type' :: T.Text
  , label :: T.Text
  }
  deriving (Show)

instance ToJSON ActionVk where
  toJSON (ActionVk type' label) =
    object
      [ "type" .= type'
      , "label" .= label
      ]

newtype ButtonVk = ButtonVk
  {action :: ActionVk}
  deriving (Show, Generic)

instance ToJSON ButtonVk

data KeyboardVk = KeyboardVk
  { one_time :: Bool
  , buttonsVk :: [[ButtonVk]]
  }
  deriving (Show)

instance ToJSON KeyboardVk where
  toJSON (KeyboardVk one_time buttonsVk) =
    object
      [ "one_time" .= one_time
      , "buttons" .= buttonsVk
      ]

buttonVk :: Int -> ButtonVk      -- Functions types for the VK keyboard.
buttonVk num =
  ButtonVk
    { action = toAction num
    }

toAction :: Int -> ActionVk
toAction num =
  ActionVk
    { type' = "text"
    , label = T.pack $ show num
    }

keyboardVk :: KeyboardVk
keyboardVk =
  KeyboardVk
    { one_time = True
    , buttonsVk = [map buttonVk [1 .. 5]]
    }

time :: IO String                             -- Get current time for the logger.
time = take 19 . show <$> getCurrentTime

file :: Handle                                -- Get Handle for the logfile.
file  = unsafePerformIO $ openFile "../log.log" AppendMode

writingLine :: Priority -> String -> IO () -- Function writes log
writingLine lvl str =                      --       information down.
  if (lvl >= logLevel) 
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
    DEBUG -> "DEBUG  "
    INFO -> "INFO   "
    WARNING -> "WARNING"
    ERROR -> "ERROR  "

getConfiguration :: String -> Either String Configuration
getConfiguration fileName =              -- Function reads configuration
  unsafePerformIO $ do                   --  information from file.
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

errorConfig :: Configuration   -- The object is used when the configuration
errorConfig =                  --   file is read unsuccessfully.
  Configuration 
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

configuration :: Configuration              -- Try to read configuration file.
configuration =
  case getConfiguration "../config.json" of
    Right v -> v
    Left e  -> errorConfig

currentMessenger :: T.Text                         -- Selected messenger.
currentMessenger = messenger configuration

myHost :: String
myHost = case currentMessenger of               -- The host of selected messenger.
  "TG" -> T.unpack $ hostTG configuration
  _ -> T.unpack $ hostVK configuration

myToken :: String                              -- The token of selected messenger.
myToken = case currentMessenger of
  "TG" -> T.unpack $ tokenTG configuration
  _ -> T.unpack $ tokenVK configuration

messengerHost :: String
messengerHost = myHost ++ "/bot"

logLevel :: Priority                             -- Logging level.
logLevel =  priorityLevel configuration

type UpdateID = Int

createStringGetUpdates :: Int -> String -- Update request string for Telegram.
createStringGetUpdates num =
  mconcat ["/getUpdates?offset=", show num, "&timeout=1"]

stringRequest :: String -> Request -- Request generation.
stringRequest str =
  parseRequest_ $
    case currentMessenger of
      "TG" -> mconcat ["https://", messengerHost, myToken, str]
      _ ->
        mconcat
          [ "https://"
          , myHost
          , "/method/messages.send?user_id="
          , str
          , "&peer_id=-"
          , show $ groupIdVK configuration
          , "&access_token="
          , myToken
          , "&v="
          , T.unpack $ apiVKVersion configuration
          ]

message' :: MessageDate -> Message
message'  = message 

forwardMessagesVk :: Int -> MessageDate -> Request -- Forming a request
forwardMessagesVk randomId' obj =                  --   to return a message
  stringRequest $                                  --      for VK.
    mconcat
      [ userId
      , "&random_id="
      , show randomId'
      , "&forward_messages="
      , messId
      ]
 where
  userId = T.unpack $ username $ chat $ message' obj
  messId = show $ message_id $ message' obj
randomId :: IO Int                             -- Generating random numbers 
randomId = randomRIO (1, 1000000)              --   for VK requests. 

repeatMessageVk :: MessageDate -> IO (Response LC.ByteString) -- Sending a
repeatMessageVk obj = do              -- request to VK to return a message.
  r <- randomId
  let string = forwardMessagesVk r obj
  writingLine DEBUG $ show string
  httpLBS $ string

sandRepeats :: MessageDate -> Environment -> IO ()   -- Sending repetitions of
sandRepeats obj env =                                --    a message.
  replicateM_ num $                                  -- Repeat the action num times.
    case currentMessenger of
      "TG" -> do
        writingLine DEBUG $ show string
        httpLBS $ string
      _ -> repeatMessageVk obj
 where
  num = getNumRepeats obj env           -- Getting the number of repeats for a current user.
  messageId = show $ message_id $ message' obj
  chatId = show $ Lib.id $ chat $ message' obj
  string =
    stringRequest $                    -- Request for Telegram.
      mconcat
        [ "/copyMessage?chat_id="
        , chatId
        , "&from_chat_id="
        , chatId
        , "&message_id="
        , messageId
        ]

data WorkHandle m a = WorkHandle -- Handle Pattern
  { writingLine' :: Priority -> String -> m a
  , sendKeyboard' :: MessageDate -> Environment -> m (Response LC.ByteString)
  , sendComment' :: MessageDate -> String -> m (Response LC.ByteString)
  , sandRepeats' :: MessageDate -> Environment -> m a
  , wordIsRepeat' ::
      WorkHandle m a ->
      (Int -> Maybe WholeObject) ->
      MessageDate ->
      [MessageDate] ->
      StateT Environment m a
  , pureOne :: StateT Environment m a
  , pureTwo :: StateT Environment m a
  }

handler :: WorkHandle IO () -- Handle for work of echobot
handler =
  WorkHandle
    { writingLine' = writingLine
    , sendKeyboard' = sendKeyboard
    , sendComment' = sendComment
    , sandRepeats' = sandRepeats
    , wordIsRepeat' = wordIsRepeat
    , pureOne = pure ()
    , pureTwo = pure ()
    }

ifKeyWord ::                     -- Keyword search and processing.
  Monad m =>
  WorkHandle m a ->
  (Int -> Maybe WholeObject) ->
  MessageDate ->
  StateT Environment m a
ifKeyWord handler getDataVk obj = do  
  env <- get
  let Just arr = result <$> fun
      fun = case currentMessenger of
        "TG" -> evalState getData env
        _ -> getDataVk $ lastUpdate env
      newObj = Prelude.head arr
      newEnv = Environment (1 + update_id newObj) (userData env)
      Just val = textM $ message' newObj
      usrName = T.unpack $ username $ chat $ message' obj
  case (textM $ message' obj) of
    Just "/repeat" -> do
      lift $ writingLine' handler INFO $ "Received /repeat from " ++ usrName
      lift $ sendKeyboard' handler obj env
      wordIsRepeat' handler handler getDataVk obj arr
    Just "/help" -> do
      lift $ writingLine' handler INFO $ "Received /help from " ++ usrName
      lift $ sendComment' handler obj $ T.unpack $ mconcat $ helpMess configuration
      pureOne handler
    _ -> do
      lift $ sandRepeats' handler obj env
      pureTwo handler

                                          -- Changing the number of repetitions.
wordIsRepeat ::
  Monad m =>
  WorkHandle m a ->
  (Int -> Maybe WholeObject) ->
  MessageDate ->
  [MessageDate] ->
  StateT Environment m a
wordIsRepeat handler getDataVk obj [] = do     -- getDataVk needed to get  
  env <- get                                   --   updates from VK.
  let Just newArr = result <$> fun
      fun = case currentMessenger of
        "TG" -> evalState getData env
        _ -> getDataVk $ lastUpdate env
  wordIsRepeat' handler handler getDataVk obj newArr
wordIsRepeat handler getDataVk obj (x : xs) = do
  env <- get
  let newObj = x
      newEnv = Environment (num + update_id newObj) (userData env)
      Just val = textM $ message' newObj
      usrName = username $ chat $ message' obj
      newUsrName = username $ chat $ message' newObj
      num = if currentMessenger == "TG" then 1 else 0
  case (usrName == newUsrName) of -- We check that the message came from the user
    True ->                       --  who requested a change in the number of repetitions.      
      case (elem val ["1", "2", "3", "4", "5"]) of
        True -> do
          lift $
            sendComment' handler obj $
              "Done! Set up "
                ++ T.unpack val
                ++ " repeat(s)."
          lift $
            writingLine' handler INFO $
              "Set up "
                ++ T.unpack val
                ++ " repeat(s) to "
                ++ T.unpack usrName
          put $
            Environment
              (num + update_id newObj)
              ( Map.insert
                  usrName
                  (read $ T.unpack val)
                  (userData env)
              )
          pureOne handler
        _ -> do
          lift $ sandRepeats' handler newObj newEnv
          put newEnv
          pureTwo handler
    _ -> do
      ifKeyWord handler getDataVk newObj
      put newEnv
      wordIsRepeat' handler handler getDataVk obj xs

toKeyboardButton :: Int -> KeyboardButton
toKeyboardButton num = KeyboardButton{text = T.pack $ show num}

createKeyboard :: ReplyKeyboardMarkup
createKeyboard =
  ReplyKeyboardMarkup
    { keyboard = [map toKeyboardButton [1 .. 5]]
    , resize_keyboard = True
    , one_time_keyboard = True
    }

question :: MessageDate -> Environment -> String
question obj env =
  mconcat
    [ "Currently set to "
    , show $ getNumRepeats obj env
    , " repetitions.\n"
    , T.unpack $ repeatMess configuration
    ]

stringToUrl :: String -> String
stringToUrl = Prelude.foldl fun ""
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
  randomId' <- randomId
  let string =
        stringRequest $
          mconcat $
            case currentMessenger of
              "TG" ->
                [ "/sendMessage?chat_id="
                , show $ Lib.id $ chat $ message' obj
                , "&text="
                , stringToUrl $ question obj env
                , "&reply_markup="
                , stringToUrl $ LC.unpack $ encode createKeyboard
                ]
              _ ->
                [ userId
                , "&random_id="
                , show randomId'
                , "&message="
                , stringToUrl $ question obj env
                , "&keyboard="
                , stringToUrl $ LC.unpack $ encode keyboardVk
                ]
      userId = T.unpack $ username $ chat $ message' obj
  writingLine DEBUG $ show string
  httpLBS string

sendComment :: MessageDate -> String -> IO (Response LC.ByteString)
sendComment obj str = do
  randomId' <- randomId
  let string = stringRequest $
        mconcat $
          case currentMessenger of
            "TG" ->
              [ "/sendMessage?chat_id="
              , show $ Lib.id $ chat $ message' obj
              , "&text="
              , stringToUrl str
              ]
            _ ->
              [ userId
              , "&random_id="
              , show randomId'
              , "&message="
              , stringToUrl str
              ]
      userId = T.unpack $ username $ chat $ message' obj
  writingLine DEBUG $ show string
  httpLBS string

type Username   = T.Text
type NumRepeats = Int

data Environment = Environment
  { lastUpdate :: UpdateID
  , userData   :: Map.Map Username NumRepeats
  }

getNumRepeats :: MessageDate -> Environment -> Int
getNumRepeats obj env = case (Map.lookup usrName $ userData env) of
  Nothing -> defaultRepaets configuration
  Just n -> n
 where
  usrName = username $ chat $ message' obj

environment :: Environment
environment =  Environment 0 $ Map.singleton "" (defaultRepaets configuration)

connection :: Request -> Int -> IO (Response LC.ByteString)  -- Function for connecting 
connection req num = do                                      --  to the server.
    x <- try $ httpLBS req
    writingLine DEBUG $ show req
    case x of
      Left e -> do
        writingLine ERROR $ show (e :: HttpException)
        when (num == 0) $ do
          getCurrentTime >>= print
          print "Connection Failure"
          print "Trying to set a connection... "
        threadDelay 1000000
        connection req (num + 1)
      Right v -> do
        writingLine DEBUG $ show v
        when (num /= 0) $ do
          getCurrentTime >>= print
          print "Connection restored"
        pure v

getData :: State Environment (Maybe WholeObject)  -- Function for getting data from
getData =  do                                     -- Telegram's server.
  env <- get
  let req = stringRequest $ createStringGetUpdates $ lastUpdate env
      x = unsafePerformIO $ connection req 0
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

firstUpdateIDSession :: StateT Environment IO () -- Function for getting update_id 
firstUpdateIDSession =  do                       --   for the first time. (Excludes 
  env <- get                                     --   processing of messages sent 
  let (obj, newEnv) = runState getData env       --   before the program is started).         
  case obj of
    Nothing -> pure ()
    _ -> do
      lift $ getCurrentTime >>= print
      lift $ print "Connection established"
      let Just arr = result <$> obj
      if lastUpdate newEnv == 1
      then put $ Environment (update_id $ last arr) (userData newEnv)
      else put $ Environment (1 + (update_id $ last arr)) (userData newEnv)

endlessCycle :: StateT Environment IO ()  -- Main program cycle for Telegram.
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
      mapM_ (ifKeyWord handler nothing) arr
      endlessCycle
