{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Control.Applicative (Alternative ((<|>)))
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad.State.Lazy
  ( MonadState (get, put),
    MonadTrans (lift),
    StateT (runStateT),
    evalStateT,
    replicateM_,
    when,
  )
import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    Value (Object),
    decode,
    eitherDecode,
    encode,
    object,
    withObject,
    (.!=),
    (.:),
    (.:?),
  )
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Foldable (asum)
import Data.Functor.Identity (runIdentity)
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import GHC.Generics (Generic)
import Network.HTTP.Simple
  ( HttpException,
    Request,
    Response,
    getResponseBody,
    getResponseStatusCode,
    httpLBS,
    parseRequest_,
  )
import System.Random (Random (randomRIO))

-- Data type for the logger.
data Priority = DEBUG | INFO | WARNING | ERROR
  deriving (Show, Eq, Ord, Generic, FromJSON)

-- Data type for the configuration file.
data Configuration = Configuration
  { messenger :: T.Text,
    hostTG :: T.Text,
    hostVK :: T.Text,
    tokenTG :: T.Text,
    tokenVK :: T.Text,
    groupIdVK :: Int,
    apiVKVersion :: T.Text,
    helpMess :: [T.Text],
    repeatMess :: T.Text,
    defaultRepeats :: Int,
    priorityLevel :: Priority,
    logOutput :: T.Text
  }
  deriving (Show, Generic, FromJSON)

-- Data types for the Telegram answer.
data Chat = Chat
  { chat_id :: Int,
    username :: T.Text
  }
  deriving (Show)

instance FromJSON Chat where
  parseJSON (Object v) = do
    chat_id <- v .: "id"
    username <- v .: "username"
    pure $ Chat chat_id username
  parseJSON _ = mempty

errorChat :: Chat
errorChat = Chat 0 "error"

data Message = Message
  { message_id :: Int,
    chat :: Chat,
    textM :: Maybe T.Text,
    attachments :: Maybe [Media]
  }
  deriving (Show)

instance FromJSON Message where
  parseJSON (Object v) = do
    message_id <- v .: "message_id"
    chat <- v .: "chat"
    textM <- v .:? "text"
    attachments <- v .:? "attachments"
    pure $ Message message_id chat textM attachments
  parseJSON _ = mempty

errorMessage :: Message
errorMessage =
  Message
    { message_id = 0,
      chat = errorChat,
      textM = Nothing,
      attachments = Nothing
    }

data MessageDate = MessageDate
  { update_id :: Int,
    message :: Message
  }
  deriving (Show)

instance FromJSON MessageDate where
  parseJSON (Object v) = do
    update_id <- v .: "update_id"
    message <- v .:? "message" .!= errorMessage
    pure $ MessageDate update_id message
  parseJSON _ = mempty

data WholeObject = WholeObject
  { ok :: Bool,
    result :: [MessageDate]
  }
  deriving (Show, Generic, FromJSON)

-- Data type for the Telegram keyboard.
newtype KeyboardButton = KeyboardButton
  {text :: T.Text}
  deriving (Show, Generic, ToJSON)

data ReplyKeyboardMarkup = ReplyKeyboardMarkup
  { keyboard :: [[KeyboardButton]],
    resize_keyboard :: Bool,
    one_time_keyboard :: Bool
  }
  deriving (Show, Generic, ToJSON)

data Media
  = Sticker
      { type_media :: T.Text,
        sticker_id :: Int
      }
  | AudioMessage
      { type_media :: T.Text,
        link_mp3 :: T.Text
      }
  | Others
      { type_media :: T.Text,
        media_id :: Int,
        owner_id :: Int,
        url :: Maybe T.Text,
        access_key :: Maybe T.Text
      }
  deriving (Show)

instance FromJSON Media where
  parseJSON = withObject "Media" $ \v ->
    asum
      [ do
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
          obj <-
            v .: "photo" <|> v .: "video" <|> v .: "audio" <|> v .: "doc"
              <|> v .: "market"
              <|> v .: "poll"
              <|> v .: "wall"
          media_id <- obj .: "id"
          owner_id <- obj .: "owner_id"
          url <- obj .:? "url"
          access_key <- obj .:? "access_key"
          pure $ Others type_media media_id owner_id url access_key
      ]

-- Data types for the VK keyboard.
data ActionVk = ActionVk
  { typeActionVk :: T.Text,
    label :: T.Text
  }
  deriving (Show)

instance ToJSON ActionVk where
  toJSON (ActionVk typeActionVk label) =
    object
      [ "type" .= typeActionVk,
        "label" .= label
      ]

newtype ButtonVk = ButtonVk
  {action :: ActionVk}
  deriving (Show, Generic, ToJSON)

data KeyboardVk = KeyboardVk
  { one_time :: Bool,
    buttonsVk :: [[ButtonVk]]
  }
  deriving (Show)

instance ToJSON KeyboardVk where
  toJSON (KeyboardVk one_time buttonsVk) =
    object
      [ "one_time" .= one_time,
        "buttons" .= buttonsVk
      ]

-- Functions types for the VK keyboard.
buttonVk :: Int -> ButtonVk
buttonVk num =
  ButtonVk
    { action = toAction num
    }

toAction :: Int -> ActionVk
toAction num =
  ActionVk
    { typeActionVk = "text",
      label = T.pack $ show num
    }

keyboardVk :: KeyboardVk
keyboardVk =
  KeyboardVk
    { one_time = True,
      buttonsVk = [map buttonVk [1 .. 5]]
    }

-- Getting current time for the logger.
time :: IO String
time = take 19 . show <$> getCurrentTime

-- Name of the logfile.
logFile :: String
logFile = "log.log"

-- Function writes information to log.
writingLine :: Priority -> String -> IO ()
writingLine lvl str = do
  logLevel' <- logLevel
  if lvl >= logLevel'
    then do
      t <- time
      let string = t ++ " UTC   " ++ showLevel lvl ++ " - " ++ str
      out <- logOutput <$> configuration
      case out of
        "file" -> appendFile logFile $ string ++ "\n"
        _ -> print string
    else pure ()
  where
    showLevel val = case val of
      DEBUG -> "DEBUG  "
      INFO -> "INFO   "
      WARNING -> "WARNING"
      ERROR -> "ERROR  "

-- Getting information from file.
getConfiguration :: String -> IO (Either String Configuration)
getConfiguration fileName = do
  t <- time
  content <- L.readFile fileName
  let obj = eitherDecode content
  case obj of
    Right _ -> pure obj
    Left e -> do
      let str = t ++ " UTC   " ++ "ERROR  " ++ " - " ++ e
      print str
      appendFile logFile $ str ++ "\n"
      pure obj

-- The object is used when the configuration file is read unsuccessfully.
errorConfig :: Configuration
errorConfig =
  Configuration
    { messenger = "Error",
      hostTG = "Error",
      hostVK = "Error",
      tokenTG = "Error",
      tokenVK = "Error",
      groupIdVK = 0,
      apiVKVersion = "Error",
      helpMess = ["Error"],
      repeatMess = "Error",
      defaultRepeats = 0,
      priorityLevel = ERROR,
      logOutput = "cons"
    }

-- Trying to read configuration file.
configuration :: IO Configuration
configuration = do
  getConf <- getConfiguration "config.json"
  pure $ case getConf of
    Right v -> v
    _ -> errorConfig

-- Selected messenger.
currentMessenger :: IO T.Text
currentMessenger = messenger <$> configuration

-- The host of selected messenger.
myHost :: IO String
myHost = do
  conf <- configuration
  crntMsngr <- currentMessenger
  pure $ case crntMsngr of
    "TG" -> T.unpack $ hostTG conf
    _ -> T.unpack $ hostVK conf

-- The token of selected messenger.
myToken :: IO String
myToken = do
  conf <- configuration
  crntMsngr <- currentMessenger
  pure $ case crntMsngr of
    "TG" -> T.unpack $ tokenTG conf
    _ -> T.unpack $ tokenVK conf

messengerHost :: IO String
messengerHost = (++ "/bot") <$> myHost

-- Logging level.
logLevel :: IO Priority
logLevel = priorityLevel <$> configuration

-- Update request string for Telegram.
createStringGetUpdates :: Int -> String
createStringGetUpdates num =
  mconcat ["/getUpdates?offset=", show num, "&timeout=1"]

-- Request generation.
stringRequest :: String -> IO Request
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
          [ "https://",
            myHost',
            "/method/messages.send?user_id=",
            str,
            "&peer_id=-",
            show $ groupIdVK conf,
            "&access_token=",
            myToken',
            "&v=",
            T.unpack $ apiVKVersion conf
          ]

-- Generating random numbers for VK requests.
randomId :: IO Int
randomId = randomRIO (1, 1000000)

-- Sending a request to VK to return a message.
repeatMessageVk :: MessageDate -> IO (Response LC.ByteString)
repeatMessageVk obj = do
  r <- randomId
  let userId = T.unpack $ username $ chat $ message obj
      str = case textM $ message obj of
        Just s -> s
        _ -> ""
      arr = case attachments $ message obj of
        Just ls -> ls
        _ -> []
      add = case arr of
        [] -> ""
        _ -> case type_media $ (\(x : _) -> x) arr of
          "sticker" -> ""
          _ -> "&attachment="
  string <-
    stringRequest $
      mconcat
        [ userId,
          "&random_id=",
          show r,
          "&message=",
          stringToUrl $ T.unpack str ++ add ++ attachment arr userId
        ]
  writingLine DEBUG $ show string
  httpLBS string

-- Processing of attachments for VK.
attachment :: [Media] -> String -> String
attachment [] _ = ""
attachment (x : xs) userId = case x of
  Sticker _ n -> "&sticker_id=" ++ show n ++ attachment xs userId
  AudioMessage _ l -> T.unpack l ++ attachment xs userId
  Others t mI oI u k -> runIdentity $ do
    let lnk = case u of
          Just txt -> txt
          _ -> ""
    pure $
      if t == "doc" && userId == show oI
        then T.unpack lnk ++ "," ++ attachment xs userId
        else
          mconcat
            [ T.unpack t,
              show oI,
              "_",
              show mI,
              case k of
                Just s -> "_" ++ T.unpack s
                _ -> "",
              ",",
              attachment xs userId
            ]

-- Sending repetitions of request for Telegram.
sandRepeats :: MessageDate -> Environment -> IO ()
sandRepeats obj env = do
  crntMsngr <- currentMessenger
  string <-
    stringRequest $
      mconcat
        [ "/copyMessage?chat_id=",
          chatId,
          "&from_chat_id=",
          chatId,
          "&message_id=",
          messageId
        ]
  num <- getNumRepeats obj env -- Getting the number of repeats for a current user.                                --    a message.
  replicateM_ num $ -- Repeat the action num times.
    case crntMsngr of
      "TG" -> do
        writingLine DEBUG $ show string
        httpLBS string
      _ -> repeatMessageVk obj
  where
    messageId = show $ message_id $ message obj
    chatId = show $ chat_id $ chat $ message obj

-- Handle Pattern
data WorkHandle m a b = WorkHandle
  { writingLineH :: Priority -> String -> m a,
    sendKeyboardH :: MessageDate -> Environment -> m b,
    sendCommentH :: MessageDate -> String -> m b,
    sandRepeatsH :: MessageDate -> Environment -> m a,
    wordIsRepeatH ::
      WorkHandle m a b ->
      (Int -> m (Maybe WholeObject)) ->
      MessageDate ->
      [MessageDate] ->
      StateT Environment m a,
    currentMessengerH :: m T.Text,
    configurationH :: m Configuration,
    getDataH :: StateT Environment m (Maybe WholeObject),
    pureOne :: StateT Environment m a,
    pureTwo :: StateT Environment m a
  }

-- Handle for work of echobot.
handler :: WorkHandle IO () (Response LC.ByteString)
handler =
  WorkHandle
    { writingLineH = writingLine,
      sendKeyboardH = sendKeyboard,
      sendCommentH = sendComment,
      sandRepeatsH = sandRepeats,
      wordIsRepeatH = wordIsRepeat,
      currentMessengerH = currentMessenger,
      configurationH = configuration,
      getDataH = getData,
      pureOne = pure (),
      pureTwo = pure ()
    }

-- Keyword search and processing.
ifKeyWord ::
  Monad m =>
  WorkHandle m a b ->
  (Int -> m (Maybe WholeObject)) ->
  MessageDate ->
  StateT Environment m a
ifKeyWord WorkHandle {..} getDataVk obj = do
  env <- get
  let usrName = T.unpack $ username $ chat $ message obj
  case textM $ message obj of
    Just "/repeat" -> do
      _ <- lift $ writingLineH INFO $ "Received /repeat from " ++ usrName
      _ <- lift $ sendKeyboardH obj env
      crntMsngr <- lift currentMessengerH
      fromServer <- lift $ case crntMsngr of
        "TG" -> evalStateT getDataH env
        _ -> getDataVk . lastUpdate $ env
      let arr = case result <$> fromServer of
            Just ls -> ls
            _ -> []
      wordIsRepeatH WorkHandle {..} getDataVk obj arr
    Just "/help" -> do
      _ <- lift $ do
        _ <- writingLineH INFO $ "Received /help from " ++ usrName
        conf <- configurationH
        sendCommentH obj $ T.unpack $ mconcat $ helpMess conf
      pureOne
    _ -> do
      _ <- lift $ sandRepeatsH obj env
      pureTwo

-- Changing the number of repetitions.
wordIsRepeat ::
  Monad m =>
  WorkHandle m a b ->
  (Int -> m (Maybe WholeObject)) ->
  MessageDate ->
  [MessageDate] ->
  StateT Environment m a
wordIsRepeat WorkHandle {..} getDataVk obj [] = do
  -- getDataVk needed to get updates from VK.
  env <- get
  crntMsngr <- lift currentMessengerH
  fromServer <- lift $ case crntMsngr of
    "TG" -> evalStateT getDataH env
    _ -> getDataVk . lastUpdate $ env
  let Just newArr = result <$> fromServer
  wordIsRepeatH WorkHandle {..} getDataVk obj newArr
wordIsRepeat WorkHandle {..} getDataVk obj (x : xs) = do
  env <- get
  crntMsngr <- lift currentMessengerH
  let newObj = x
      newEnv = Environment (num + update_id newObj) (userData env)
      Just val = textM $ message newObj
      usrName = username $ chat $ message obj
      newUsrName = username $ chat $ message newObj
      num = if crntMsngr == "TG" then 1 else 0
  if usrName == newUsrName -- We check that the message came from the user
    then --  who requested a change in the number of repetitions.

      ( if val `elem` ["1", "2", "3", "4", "5"]
          then
            ( do
                _ <-
                  lift $
                    sendCommentH obj $
                      "Done! Set up "
                        ++ T.unpack val
                        ++ " repeat(s)."
                _ <-
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
            )
          else
            ( do
                _ <- lift $ sandRepeatsH newObj newEnv
                put newEnv
                pureTwo
            )
      )
    else
      ( do
          _ <- ifKeyWord WorkHandle {..} getDataVk newObj
          put newEnv
          wordIsRepeatH WorkHandle {..} getDataVk obj xs
      )

-- Creating keyboard for TG.
toKeyboardButton :: Int -> KeyboardButton
toKeyboardButton num = KeyboardButton {text = T.pack $ show num}

createKeyboard :: ReplyKeyboardMarkup
createKeyboard =
  ReplyKeyboardMarkup
    { keyboard = [map toKeyboardButton [1 .. 5]],
      resize_keyboard = True,
      one_time_keyboard = True
    }

question :: MessageDate -> Environment -> IO String
question obj env = do
  conf <- configuration
  num <- getNumRepeats obj env
  pure $
    mconcat
      [ "Currently set to ",
        show num,
        " repetitions.\n",
        T.unpack $ repeatMess conf
      ]

stringToUrl :: String -> String
stringToUrl = Prelude.foldl encodingChar ""
  where
    encodingChar acc c =
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
  let userId = T.unpack $ username $ chat $ message obj
  randomId' <- randomId
  crntMsngr <- currentMessenger
  question' <- question obj env
  string <- stringRequest $
    mconcat $
      case crntMsngr of
        "TG" ->
          [ "/sendMessage?chat_id=",
            show $ chat_id $ chat $ message obj,
            "&text=",
            stringToUrl question',
            "&reply_markup=",
            stringToUrl $ LC.unpack $ encode createKeyboard
          ]
        _ ->
          [ userId,
            "&random_id=",
            show randomId',
            "&message=",
            stringToUrl question',
            "&keyboard=",
            stringToUrl $ LC.unpack $ encode keyboardVk
          ]
  writingLine DEBUG $ show string
  httpLBS string

sendComment :: MessageDate -> String -> IO (Response LC.ByteString)
sendComment obj str = do
  let userId = T.unpack $ username $ chat $ message obj
  randomId' <- randomId
  crntMsngr <- currentMessenger
  string <- stringRequest $
    mconcat $
      case crntMsngr of
        "TG" ->
          [ "/sendMessage?chat_id=",
            show $ chat_id $ chat $ message obj,
            "&text=",
            stringToUrl str
          ]
        _ ->
          [ userId,
            "&random_id=",
            show randomId',
            "&message=",
            stringToUrl str
          ]
  writingLine DEBUG $ show string
  httpLBS string

type UpdateID = Int

type Username = T.Text

type NumRepeats = Int

data Environment = Environment
  { lastUpdate :: UpdateID,
    userData :: Map.Map Username NumRepeats
  }

getNumRepeats :: MessageDate -> Environment -> IO Int
getNumRepeats obj env = do
  conf <- configuration
  pure $ case Map.lookup usrName $ userData env of
    Nothing -> defaultRepeats conf
    Just n -> n
  where
    usrName = username $ chat $ message obj

environment :: IO Environment
environment = Environment 0 . Map.singleton "" . defaultRepeats <$> configuration

-- Function for connecting to the server.
connection :: Request -> Int -> IO (Response LC.ByteString)
connection req num = do
  x <- try $ httpLBS req
  writingLine DEBUG $ show req
  case x of
    Left e -> do
      writingLine ERROR $ show (e :: HttpException)
      when (num == 0) $ do
        getCurrentTime >>= print
        print ("Connection Failure" :: String)
        print ("Trying to set a connection... " :: String)
      threadDelay 1000000
      connection req (num + 1)
    Right v -> do
      writingLine DEBUG $ show v
      when (num /= 0) $ do
        getCurrentTime >>= print
        print ("Connection restored" :: String)
      pure v

-- Function for getting data from Telegram's server.
getData :: StateT Environment IO (Maybe WholeObject)
getData = do
  env <- get
  req <- lift $ stringRequest . createStringGetUpdates . lastUpdate $ env
  x <- lift $ connection req 0
  let code = getResponseStatusCode x
  if code == 200
    then
      ( do
          let obj = decode $ getResponseBody x
          case result <$> obj of
            Just [] -> do
              put $ Environment 1 (userData env)
              getData
            _ -> do
              pure obj
      )
    else lift $ do
      writingLine ERROR $ "statusCode " ++ show code
      pure Nothing

-- Function for getting update_id  for the first time.
--  (Excludes processing of messages sent before the program is started).
firstUpdateIDSession :: StateT Environment IO ()
firstUpdateIDSession = do
  env <- get
  (obj, newEnv) <- lift $ runStateT getData env
  case obj of
    Nothing -> pure ()
    _ -> do
      lift $ getCurrentTime >>= print
      lift $ print ("Connection established" :: String)
      let update_id' = case result <$> obj of
            Just [] -> 0
            Just md -> (\(x : _) -> update_id x) (reverse md)
            _ -> 0
      if lastUpdate newEnv == 1
        then put $ Environment update_id' (userData newEnv)
        else put $ Environment (1 + update_id') (userData newEnv)

-- Main program cycle for Telegram.
endlessCycle :: StateT Environment IO ()
endlessCycle = do
  env <- get
  obj <- lift $ evalStateT getData env
  let nothing _ = pure Nothing
  case obj of
    Nothing -> do
      lift $ writingLine ERROR "Broken request!"
      endlessCycle
    _ -> do
      let arr = case result <$> obj of
            Just [x] -> [x]
            _ -> []
          update_id' = if null arr then 0 else (\(x : _) -> update_id x) (reverse arr)
      put $ Environment (1 + update_id') (userData env)
      _ <- get
      mapM_ (ifKeyWord handler nothing) arr
      endlessCycle
