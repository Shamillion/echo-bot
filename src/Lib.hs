{-# LANGUAGE DerivingVia #-}

module Lib where

import Config
  ( Configuration (helpMess),
    currentMessenger,
    getConfiguration,
  )
import Control.Monad.State.Lazy
  ( MonadState (get, put),
    MonadTrans (lift),
    StateT,
    evalStateT,
    replicateM_,
  )
import Data.Aeson
  ( encode,
  )
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import Environment
  ( Environment (..),
    NumRepeats (..),
    UpdateID (..),
    Username (Username),
  )
import Logger.Data (Priority (..))
import Logger.Functions (writingLine)
import Network.HTTP.Simple
  ( Response,
    httpLBS,
  )
import RequestBuilding
  ( createQuestion,
    createStringRequest,
    getNumRepeats,
    stringToUrl,
  )
import System.Random (Random (randomRIO))
import Telegram.Data
  ( Chat (chat_id, username),
    Message (chat, message_id, textM),
    MessageDate (..),
    WholeObject (result),
    errorMessage,
  )
import Telegram.Functions (getData)
import Telegram.KeyboardData (createKeyboard)
import Text.Read (readMaybe)
import Vk.Functions (repeatMessageVk)
import Vk.KeyboardData (keyboardVk)

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

-- Generating random numbers for VK requests.
randomId :: IO Int
randomId = randomRIO (1, 1000000)

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
