{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Lib where

import Control.Applicative 
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Data.Foldable
import Control.Monad.State.Lazy
import Data.Aeson
import Data.Functor.Identity (runIdentity)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T hiding (last)
import Data.Time (getCurrentTime)
import GHC.Generics (Generic)
import Network.HTTP.Simple
import System.IO
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
  deriving (Show, Generic, FromJSON)

data Chat = Chat                   -- Data types for the Telegram answer.
  { id :: Int
  , username :: T.Text
  }
  deriving (Show, Generic, FromJSON)

data Message = Message
  { message_id :: Int
  , chat :: Chat
  , textM :: Maybe T.Text
  , attachments :: Maybe [Media]
  }
  deriving Show

instance FromJSON Message where
  parseJSON (Object v) = do
    message_id <- v .: "message_id"
    chat <- v .: "chat"
    textM <- v .:? "text"
    attachments <- v .:? "attachments" 
    pure $ Message message_id chat textM attachments

data MessageDate = MessageDate
  { update_id :: Int
  , message :: Message
  }
  deriving (Show, Generic, FromJSON)

data WholeObject = WholeObject
  { ok :: Bool
  , result :: [MessageDate]
  }
  deriving (Show, Generic, FromJSON)

newtype KeyboardButton = KeyboardButton -- Data type for the Telegram keyboard.
  {text :: T.Text}
  deriving (Show, Generic, ToJSON)

data ReplyKeyboardMarkup = ReplyKeyboardMarkup
  { keyboard :: [[KeyboardButton]]
  , resize_keyboard :: Bool
  , one_time_keyboard :: Bool
  }
  deriving (Show, Generic, ToJSON)

data Priority = DEBUG | INFO | WARNING | ERROR -- Data type for the logger.
  deriving (Show, Eq, Ord, Generic, FromJSON)

data Media =  Sticker { type_media :: T.Text
                      , sticker_id :: Int
                      } 
            | AudioMessage  { type_media :: T.Text
                            , link_mp3 :: T.Text 
                            }                    
            | Others  { type_media :: T.Text
                      , media_id :: Int
                      , owner_id :: Int
                      , url :: Maybe T.Text
                      , access_key :: Maybe T.Text
                      }
  deriving Show

instance FromJSON Media where
    parseJSON = withObject "Media" $ \v -> asum [
      do 
        type_media <- v .: "type"
        obj <- v .: "sticker"
        sticker_id <- obj .: "sticker_id"
        pure $ Sticker type_media sticker_id,
      do
        type_media <- v .: "type"
        obj <- v .: "audio_message"
        link_mp3 <- obj .: "link_mp3"
        pure $ AudioMessage type_media link_mp3,        
      do
        type_media <- v .: "type"
        obj <- v .: "photo" <|> v .: "video" <|> v .: "audio" <|> v .: "doc"
                            <|> v .: "market" <|> v .: "poll" <|> v .: "wall"
        media_id <- obj .: "id"
        owner_id <- obj .: "owner_id"
        url <- obj .:? "url"
        access_key <- obj .:? "access_key"
        pure $ Others type_media media_id owner_id url access_key
        ]

data ActionVk = ActionVk -- Data types for the VK keyboard.
  { type' :: T.Text
  , label :: T.Text
  }
  deriving Show

instance ToJSON ActionVk where
  toJSON (ActionVk type' label) =
    object
      [ "type" .= type'
      , "label" .= label
      ]

newtype ButtonVk = ButtonVk
  {action :: ActionVk}
  deriving (Show, Generic, ToJSON)

data KeyboardVk = KeyboardVk
  { one_time :: Bool
  , buttonsVk :: [[ButtonVk]]
  }
  deriving Show

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

file :: IO Handle                                -- Get Handle for the logfile.
file  = openFile "../log.log" AppendMode

writingLine :: Priority -> String -> IO () -- Function writes log
writingLine lvl str = do                     --       information down.
  logLevel' <- logLevel
  if (lvl >= logLevel') 
    then do
      t <- time
      let string = t ++ " UTC   " ++ fun lvl ++ " - " ++ str
      out <- logOutput <$> configuration
      case out of
        "file" -> do
          file' <- file
          hPutStrLn file' string
          hFlush file'
        _ -> print string
    else pure ()
 where  
  fun val = case val of
    DEBUG -> "DEBUG  "
    INFO -> "INFO   "
    WARNING -> "WARNING"
    ERROR -> "ERROR  "

getConfiguration :: String -> IO (Either String Configuration)
getConfiguration fileName = do                   --  information from file.
    t <- time
    content <- L.readFile fileName
    let obj = eitherDecode content
    case obj of
      Right _ -> pure obj
      Left e  -> do
        let str = t ++ " UTC   " ++ "ERROR  " ++ " - " ++ e
        print str
        file' <- file
        hPutStrLn file' str
        hFlush file'
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

configuration :: IO Configuration              -- Try to read configuration file.
configuration = do
  getConf <- getConfiguration "config.json"
  pure $ case getConf of
    Right v -> v
    Left e  -> errorConfig

currentMessenger :: IO T.Text                         -- Selected messenger.
currentMessenger = messenger <$> configuration                                

myHost :: IO String                                                           
myHost = do
  conf <- configuration
  crntMsngr <- currentMessenger
  pure $ case crntMsngr of               -- The host of selected messenger.
    "TG" -> T.unpack $ hostTG conf
    _ -> T.unpack $ hostVK conf

myToken :: IO String                              -- The token of selected messenger.
myToken = do                                                                  
  conf <- configuration
  crntMsngr <- currentMessenger
  pure $ case crntMsngr of
    "TG" -> T.unpack $ tokenTG conf
    _ -> T.unpack $ tokenVK conf

messengerHost :: IO String                                               
messengerHost = (++ "/bot") <$> myHost

logLevel :: IO Priority                             -- Logging level.         
logLevel =  priorityLevel <$> configuration

type UpdateID = Int

createStringGetUpdates :: Int -> String -- Update request string for Telegram.
createStringGetUpdates num =
  mconcat ["/getUpdates?offset=", show num, "&timeout=1"]

stringRequest :: String -> IO Request -- Request generation.                   
stringRequest str = do
  conf <- configuration
  crntMsngr <- currentMessenger
  myHost' <- myHost
  messengerHost' <- messengerHost 
  myToken' <- myToken
  pure . parseRequest_ $
    case crntMsngr of
      "TG" -> mconcat ["https://", messengerHost', myToken', str]
      _ ->
        mconcat
          [ "https://"
          , myHost'
          , "/method/messages.send?user_id="
          , str
          , "&peer_id=-"
          , show $ groupIdVK conf
          , "&access_token="
          , myToken'
          , "&v="
          , T.unpack $ apiVKVersion conf
          ]

message' :: MessageDate -> Message
message'  = message 

forwardMessagesVk :: Int -> MessageDate -> IO Request -- Forming a request
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
  let userId = T.unpack $ username $ chat $ message' obj
      Just str = textM $ message' obj
      Just arr = attachments $ message' obj 
      add = case arr of
              [] -> ""
              _  -> case type_media $ head arr of
                      "sticker" -> ""
                      _ -> "&attachment="           
  string <- stringRequest $
        mconcat $
          [ userId
          , "&random_id="
          , show r
          , "&message="
          , stringToUrl $ T.unpack str ++ add ++ attachment arr userId                    
          ]   
  writingLine DEBUG $ show string
  httpLBS $ string

attachment :: [Media] -> String -> String  -- Processing of attachments for VK.
attachment [] _ = ""
attachment (x:xs) userId = case x of
  Sticker t n -> "&sticker_id=" ++ show n ++ attachment xs userId
  AudioMessage _ l -> T.unpack l ++ attachment xs userId
  Others t mI oI u k -> runIdentity $ do
    let Just lnk = u
    pure $ if t == "doc" && userId == show oI
      then T.unpack lnk ++ "," ++ attachment xs userId 
      else mconcat 
            [ T.unpack t
            , show oI
            , "_"
            , show mI
            , case k of
                Just s -> "_" ++ T.unpack s
                _ -> ""
            , ","
            , attachment xs userId 
            ]

sandRepeats :: MessageDate -> Environment -> IO ()   -- Sending repetitions of
sandRepeats obj env = do
  crntMsngr <- currentMessenger  
  string <- stringRequest $                    -- Request for Telegram.
    mconcat
      [ "/copyMessage?chat_id="
      , chatId
      , "&from_chat_id="
      , chatId
      , "&message_id="
      , messageId
      ] 
  num <- getNumRepeats obj env           -- Getting the number of repeats for a current user.                                --    a message.
  replicateM_ num $                                  -- Repeat the action num times.
    case crntMsngr of
      "TG" -> do
        writingLine DEBUG $ show string
        httpLBS $ string
      _ -> repeatMessageVk obj
 where   
  messageId = show $ message_id $ message' obj
  chatId = show $ Lib.id $ chat $ message' obj


data WorkHandle m a = WorkHandle -- Handle Pattern
  { writingLineH :: Priority -> String -> m a
  , sendKeyboard' :: MessageDate -> Environment -> m (Response LC.ByteString)
  , sendComment' :: MessageDate -> String -> m (Response LC.ByteString)
  , sandRepeats' :: MessageDate -> Environment -> m a
  , wordIsRepeat' ::
      WorkHandle m a ->
      (Int -> m (Maybe WholeObject)) ->
      MessageDate ->
      [MessageDate] ->
      StateT Environment m a
  , currentMessengerH :: m T.Text 
  , configurationH :: m Configuration 
  , getDataH :: StateT Environment m (Maybe WholeObject)  
  , pureOne :: StateT Environment m a
  , pureTwo :: StateT Environment m a
  }

handler :: WorkHandle IO () -- Handle for work of echobot
handler =
  WorkHandle
    { writingLineH = writingLine
    , sendKeyboard' = sendKeyboard
    , sendComment' = sendComment
    , sandRepeats' = sandRepeats
    , wordIsRepeat' = wordIsRepeat
    , currentMessengerH = currentMessenger
    , configurationH = configuration
    , getDataH = getData
    , pureOne = pure ()
    , pureTwo = pure ()
    }

ifKeyWord ::                     -- Keyword search and processing.
  Monad m =>
  WorkHandle m a ->
  (Int -> m (Maybe WholeObject)) ->
  MessageDate ->
  StateT Environment m a
ifKeyWord WorkHandle {..} getDataVk obj = do  
  env <- get
  crntMsngr <- lift $ currentMessengerH 
  fun <- lift $ case crntMsngr of
    "TG" -> evalStateT getDataH env
    _ -> getDataVk . lastUpdate $ env
  let Just arr = result <$> fun   
      newObj = Prelude.head arr
      newEnv = Environment (1 + update_id newObj) (userData env)
      Just val = textM $ message' newObj
      usrName = T.unpack $ username $ chat $ message' obj
  case (textM $ message' obj) of
    Just "/repeat" -> do
      lift $ writingLineH INFO $ "Received /repeat from " ++ usrName
      lift $ sendKeyboard' obj env
      wordIsRepeat WorkHandle {..} getDataVk obj arr
    Just "/help" -> do
      lift $ writingLineH INFO $ "Received /help from " ++ usrName
      lift $ do
        conf <- configurationH 
        sendComment' obj $ T.unpack $ mconcat $ helpMess conf
      pureOne 
    _ -> do
      lift $ sandRepeats' obj env
      pureTwo 

                                          -- Changing the number of repetitions.
wordIsRepeat ::
  Monad m =>
  WorkHandle m a ->
  (Int -> m (Maybe WholeObject)) ->
  MessageDate ->
  [MessageDate] ->
  StateT Environment m a
wordIsRepeat WorkHandle {..} getDataVk obj [] = do     -- getDataVk needed to get  
  env <- get                                   --   updates from VK.
  crntMsngr <- lift $ currentMessengerH   
  fun <- lift $ case crntMsngr of
    "TG" -> evalStateT getDataH env
    _ -> getDataVk . lastUpdate $ env
  let Just newArr = result <$> fun      
  wordIsRepeat WorkHandle {..} getDataVk obj newArr
wordIsRepeat WorkHandle {..} getDataVk obj (x : xs) = do
  env <- get
  crntMsngr <- lift $ currentMessengerH 
  let newObj = x
      newEnv = Environment (num + update_id newObj) (userData env)
      Just val = textM $ message' newObj
      usrName = username $ chat $ message' obj
      newUsrName = username $ chat $ message' newObj
      num = if crntMsngr == "TG" then 1 else 0
  case (usrName == newUsrName) of -- We check that the message came from the user
    True ->                       --  who requested a change in the number of repetitions.      
      case (elem val ["1", "2", "3", "4", "5"]) of
        True -> do
          lift $
            sendComment' obj $
              "Done! Set up "
                ++ T.unpack val
                ++ " repeat(s)."
          lift $
            writingLineH INFO $
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
          pureOne 
        _ -> do
          lift $ sandRepeats' newObj newEnv
          put newEnv
          pureTwo 
    _ -> do
      ifKeyWord WorkHandle {..} getDataVk newObj
      put newEnv
      wordIsRepeat WorkHandle {..} getDataVk obj xs

toKeyboardButton :: Int -> KeyboardButton
toKeyboardButton num = KeyboardButton{text = T.pack $ show num}

createKeyboard :: ReplyKeyboardMarkup
createKeyboard =
  ReplyKeyboardMarkup
    { keyboard = [map toKeyboardButton [1 .. 5]]
    , resize_keyboard = True
    , one_time_keyboard = True
    }

question :: MessageDate -> Environment -> IO String                  
question obj env = do
  conf <- configuration
  num <- getNumRepeats obj env
  pure $ mconcat
    [ "Currently set to "
    , show num 
    , " repetitions.\n"
    , T.unpack $ repeatMess conf
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
  let userId = T.unpack $ username $ chat $ message' obj
  randomId' <- randomId
  crntMsngr <- currentMessenger
  question' <- question obj env
  string <- stringRequest $
          mconcat $
            case crntMsngr of
              "TG" ->
                [ "/sendMessage?chat_id="
                , show $ Lib.id $ chat $ message' obj
                , "&text="
                , stringToUrl question'
                , "&reply_markup="
                , stringToUrl $ LC.unpack $ encode createKeyboard
                ]
              _ ->
                [ userId
                , "&random_id="
                , show randomId'
                , "&message="
                , stringToUrl question'
                , "&keyboard="
                , stringToUrl $ LC.unpack $ encode keyboardVk
                ]
  writingLine DEBUG $ show string
  httpLBS string

sendComment :: MessageDate -> String -> IO (Response LC.ByteString)
sendComment obj str = do
  let userId = T.unpack $ username $ chat $ message' obj
  randomId' <- randomId
  crntMsngr <- currentMessenger
  string <- stringRequest $
        mconcat $
          case crntMsngr of
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
  writingLine DEBUG $ show string
  httpLBS string

type Username   = T.Text
type NumRepeats = Int

data Environment = Environment
  { lastUpdate :: UpdateID
  , userData   :: Map.Map Username NumRepeats
  }

getNumRepeats :: MessageDate -> Environment -> IO Int
getNumRepeats obj env = do
  conf <- configuration
  pure $ case (Map.lookup usrName $ userData env) of
    Nothing -> defaultRepaets conf
    Just n -> n
 where
  usrName = username $ chat $ message' obj

environment :: IO Environment                                            
environment = do
  conf <- configuration
  pure $ Environment 0 $ Map.singleton "" (defaultRepaets conf)

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

getData :: StateT Environment IO (Maybe WholeObject)  -- Function for getting data from
getData =  do                                     -- Telegram's server.
  env <- get
  req <- lift $ stringRequest . createStringGetUpdates . lastUpdate $ env
  x <- lift $ connection req 0
  let code = getResponseStatusCode x
  lift . writingLine ERROR $ "statusCode" ++ show code
  case code == 200 of
    False -> lift $ do
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
  (obj, newEnv) <- lift $ runStateT getData env       --   before the program is started).         
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
  obj <- lift $ evalStateT getData env
  let nothing num = pure Nothing
  case obj of
    Nothing -> lift $ writingLine ERROR "Broken request!"
    _ -> do
      let Just arr = result <$> obj
      put $ Environment (1 + (update_id $ last arr)) (userData env)
      newEnv <- get
      mapM_ (ifKeyWord handler nothing) arr
      endlessCycle
