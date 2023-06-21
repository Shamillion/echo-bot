{-# LANGUAGE DerivingVia #-}

module Lib where

import Config
  ( Configuration (helpMess, messenger),
  )
import Control.Monad.State.Lazy
  ( MonadState (get, put),
    MonadTrans (lift),
    StateT,
    replicateM_,
  )
import Data.Aeson
  ( encode,
  )
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Map.Lazy as Map
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
  let conf = configuration env
      crntMsngr = messenger conf
      string =
        createStringRequest conf $
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
        writingLine conf DEBUG $ show string
        httpLBS string
      _ -> repeatMessageVk conf obj
  where
    messageId = show $ message_id $ message obj
    chatId = show $ chat_id $ chat $ message obj

data Command = Repeat Int | Help | Report String
  deriving (Eq, Show)

-- Handle Pattern
data WorkHandle m a b = WorkHandle
  { writingLineH :: Configuration -> Priority -> String -> m a,
    sendKeyboardH :: MessageDate -> Environment -> m b,
    sendCommentH :: Configuration -> MessageDate -> String -> m b,
    sendRepeatsH :: MessageDate -> Environment -> m a,
    wordIsRepeatH ::
      WorkHandle m a b ->
      (UpdateID -> m (Maybe WholeObject)) ->
      MessageDate ->
      [MessageDate] ->
      StateT Environment m Command,
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
  let usrName = username $ chat $ message obj
      conf = configuration env
  case textM $ message obj of
    Just "/repeat" -> do
      _ <- lift $ writingLineH conf INFO $ "Received /repeat from " ++ usrName
      _ <- lift $ sendKeyboardH obj env
      fromServer <- case messenger conf of
        "TG" -> getDataH
        _ -> lift . getDataVk . lastUpdate $ env
      let arr = case result <$> fromServer of
            Just ls -> ls
            _ -> []
      wordIsRepeatH WorkHandle {..} getDataVk obj arr
    Just "/help" -> do
      _ <- lift $ do
        _ <- writingLineH conf INFO $ "Received /help from " ++ usrName
        sendCommentH conf obj $ mconcat $ helpMess conf
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
  let conf = configuration env
  fromServer <- case messenger conf of
    "TG" -> getDataH
    _ -> lift . getDataVk . lastUpdate $ env
  newArr <- lift $
    case result <$> fromServer of
      Just n -> pure n
      Nothing ->
        writingLineH conf ERROR "The array of messages is missing"
          >> pure [MessageDate 0 errorMessage]
  wordIsRepeatH WorkHandle {..} getDataVk obj newArr
wordIsRepeat WorkHandle {..} getDataVk obj (x : xs) = do
  env <- get
  let conf = configuration env
      newObj = x
      newEnv = Environment (num + update_id newObj) (userData env) (configuration env)
      usrNameText = username . chat . message $ obj
      usrName = Username usrNameText
      newUsrName = Username . username . chat . message $ newObj
      num = UpdateID $ if messenger conf == "TG" then 1 else 0
  val <- lift $
    case textM (message newObj) >>= readMaybe of
      Just n -> pure n
      Nothing -> writingLineH conf ERROR "No parse NumRepeats from message" >> pure 0
  if usrName == newUsrName -- We check that the message came from the user
    then --  who requested a change in the number of repetitions.

      ( if val `elem` [1 .. 5]
          then
            ( do
                _ <-
                  lift $
                    sendCommentH conf obj $
                      "Done! Set up "
                        ++ show val
                        ++ " repeat(s)."
                _ <-
                  lift $
                    writingLineH conf INFO $
                      "Set up "
                        ++ show val
                        ++ " repeat(s) to "
                        ++ usrNameText
                put $
                  Environment
                    (num + update_id newObj)
                    ( Map.insert
                        usrName
                        (NumRepeats val)
                        (userData env)
                    )
                    (configuration env)
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
  randomId' <- randomId
  question' <- createQuestion obj env
  let userId = username $ chat $ message obj
      conf = configuration env
      crntMsngr = messenger conf
      string = createStringRequest conf $
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
  writingLine conf DEBUG $ show string
  httpLBS string

sendComment :: Configuration -> MessageDate -> String -> IO (Response LC.ByteString)
sendComment conf obj str = do
  randomId' <- randomId
  let userId = username $ chat $ message obj
      string = createStringRequest conf $
        mconcat $
          case messenger conf of
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
  writingLine conf DEBUG $ show string
  httpLBS string
