{-# LANGUAGE DerivingVia #-}

module Lib where

import Config
  ( Configuration
      ( apiVKVersion,
        defaultRepeats,
        groupIdVK,
        helpMess,
        repeatMess
      ),
    Priority (..),
    getConfiguration,
    currentMessenger,
    messengerHost,
    myHost,
    myToken,
    writingLine,
  )
import Control.Monad.State.Lazy
  ( MonadState (get, put),
    MonadTrans (lift),
    StateT (runStateT),
    evalStateT,
    replicateM_,
  )
import Data.Aeson
  ( encode,
  )
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Functor.Identity (runIdentity)
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Environment
  ( Environment (..),
    NumRepeats (..),
    UpdateID (..),
    Username (Username),
  )
import KeyboardData
  ( createKeyboard,
    keyboardVk,
  )
import Network.HTTP.Simple
  ( Request,
    Response,
    httpLBS,
    parseRequest_,
  )
import System.Random
  ( Random (randomRIO),
  )
import Telegram
  ( Chat (chat_id, username),
    Media (AudioMessage, Others, Sticker, type_media),
    Message (attachments, chat, message_id, textM),
    MessageDate (..),
    WholeObject (result),
    errorMessage,
    getData,
  )
import Text.Read (readMaybe)

-- Request generation.
createStringRequest :: String -> IO Request
createStringRequest str = do
  conf <- getConfiguration
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
    createStringRequest $
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

-- Sending repetitions of request.
sendRepeats :: MessageDate -> Environment -> IO ()
sendRepeats obj env = do
  crntMsngr <- currentMessenger
  string <-
    createStringRequest $
      mconcat
        [ "/copyMessage?chat_id=",
          chatId,
          "&from_chat_id=",
          chatId,
          "&message_id=",
          messageId
        ]
  NumRepeats num <- getNumRepeats obj env -- Getting the number of repeats for a current user.                                --    a message.
  replicateM_ num $ -- Repeat the action num times.
    case crntMsngr of
      "TG" -> do
        writingLine DEBUG $ show string
        httpLBS string
      _ -> repeatMessageVk obj
  where
    messageId = show $ message_id $ message obj
    chatId = show $ chat_id $ chat $ message obj

data Command = Repeat Int | Help | Report String
  deriving (Eq, Show)

-- Handle Pattern
data WorkHandle m a b = WorkHandle
  { writingLineH :: Priority -> String -> m a,
    sendKeyboardH :: MessageDate -> Environment -> m b,
    sendCommentH :: MessageDate -> String -> m b,
    sendRepeatsH :: MessageDate -> Environment -> m a,
    wordIsRepeatH ::
      WorkHandle m a b ->
      (UpdateID -> m (Maybe WholeObject)) ->
      MessageDate ->
      [MessageDate] ->
      StateT Environment m Command,
    currentMessengerH :: m T.Text,
    getConfigurationH :: m Configuration,
    getDataH :: StateT Environment m (Maybe WholeObject)
  }

-- Handle for work of echobot.
handler :: WorkHandle IO () (Response LC.ByteString)
handler =
  WorkHandle
    { writingLineH = writingLine,
      sendKeyboardH = sendKeyboard,
      sendCommentH = sendComment,
      sendRepeatsH = sendRepeats,
      wordIsRepeatH = wordIsRepeat,
      currentMessengerH = currentMessenger,
      getConfigurationH = getConfiguration,
      getDataH = getData
    }

-- Keyword search and processing.
ifKeyWord ::
  Monad m =>
  WorkHandle m a b ->
  (UpdateID -> m (Maybe WholeObject)) ->
  MessageDate ->
  StateT Environment m Command
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
        conf <- getConfigurationH
        sendCommentH obj $ T.unpack $ mconcat $ helpMess conf
      pure Help
    _ -> do
      _ <- lift $ sendRepeatsH obj env
      pure $ Report "not a keyword"

-- Changing the number of repetitions.
wordIsRepeat ::
  Monad m =>
  WorkHandle m a b ->
  (UpdateID -> m (Maybe WholeObject)) ->
  MessageDate ->
  [MessageDate] ->
  StateT Environment m Command
wordIsRepeat WorkHandle {..} getDataVk obj [] = do
  -- getDataVk needed to get updates from VK.
  env <- get
  crntMsngr <- lift currentMessengerH
  fromServer <- lift $ case crntMsngr of
    "TG" -> evalStateT getDataH env
    _ -> getDataVk . lastUpdate $ env
  newArr <- lift $
    case result <$> fromServer of
      Just n -> pure n
      Nothing ->
        writingLineH ERROR "The array of messages is missing"
          >> pure [MessageDate 0 errorMessage]
  wordIsRepeatH WorkHandle {..} getDataVk obj newArr
wordIsRepeat WorkHandle {..} getDataVk obj (x : xs) = do
  env <- get
  crntMsngr <- lift currentMessengerH
  let newObj = x
      newEnv = Environment (num + update_id newObj) (userData env)
      usrNameText = username . chat . message $ obj
      usrName = Username usrNameText
      newUsrName = Username . username . chat . message $ newObj
      num = UpdateID $ if crntMsngr == "TG" then 1 else 0
  val <- lift $
    case textM (message newObj) >>= readMaybe . T.unpack of
      Just n -> pure n
      Nothing -> writingLineH ERROR "No parse NumRepeats from message" >> pure 0
  if usrName == newUsrName -- We check that the message came from the user
    then --  who requested a change in the number of repetitions.

      ( if val `elem` [1 .. 5]
          then
            ( do
                _ <-
                  lift $
                    sendCommentH obj $
                      "Done! Set up "
                        ++ show val
                        ++ " repeat(s)."
                _ <-
                  lift $
                    writingLineH INFO $
                      "Set up "
                        ++ show val
                        ++ " repeat(s) to "
                        ++ T.unpack usrNameText
                put $
                  Environment
                    (num + update_id newObj)
                    ( Map.insert
                        usrName
                        (NumRepeats val)
                        (userData env)
                    )
                pure $ Repeat val
            )
          else
            ( do
                _ <- lift $ sendRepeatsH newObj newEnv
                put newEnv
                pure $ Report "number out of range"
            )
      )
    else
      ( do
          _ <- ifKeyWord WorkHandle {..} getDataVk newObj
          put newEnv
          wordIsRepeatH WorkHandle {..} getDataVk obj xs
      )

createQuestion :: MessageDate -> Environment -> IO String
createQuestion obj env = do
  conf <- getConfiguration
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
  question' <- createQuestion obj env
  string <- createStringRequest $
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
  string <- createStringRequest $
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

getNumRepeats :: MessageDate -> Environment -> IO NumRepeats
getNumRepeats obj env = do
  conf <- getConfiguration
  pure $ case Map.lookup usrName $ userData env of
    Nothing -> NumRepeats $ defaultRepeats conf
    Just n -> n
  where
    usrName = Username . username $ chat $ message obj

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
      lift $ putStrLn "Connection established"
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
