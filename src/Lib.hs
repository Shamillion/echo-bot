{-# LANGUAGE DerivingVia #-}

module Lib where

import Config
  ( Configuration
      ( messageHelpCommand
      ),
  )
import Control.Monad.State.Lazy
  ( StateT,
    get,
    put,
  )
import Data
  ( Chat (username),
    Message (chat, textM),
    MessageDate (..),
    DataFromServer (result),
    errorMessage,
  )
import qualified Data.Map.Lazy as Map
import Environment
  ( Environment (..),
    NumRepeats (..),
    UpdateID (..),
    Username (Username),
  )
import Logger.Data (Priority (..))
import Network.HTTP.Simple (Request)
import RequestBuilding
  ( createQuestion,
    createStringRequest,
    getNumRepeats,
  )
import Text.Read (readMaybe)

sendRepeats :: Monad m => WorkHandle m a b -> MessageDate -> StateT Environment m a
sendRepeats WorkHandle {..} obj = do
  env <- get
  let NumRepeats num = getNumRepeats obj env -- Getting the number of repeats for a current user.                                --    a message.
  repeatActionH num $ repeatMessageH obj -- Repeat the action num times.

data Command = Repeat Int | Help | Report String
  deriving (Eq, Show)

data WorkHandle m a b = WorkHandle
  { writingLineH :: Priority -> String -> StateT Environment m a,
    getDataH :: StateT Environment m (Maybe DataFromServer),
    addNumberH :: UpdateID,
    stringForCreateKeyboardH :: MessageDate -> String -> String,
    stringCommentH :: MessageDate -> String -> String,
    repeatMessageH :: MessageDate -> StateT Environment m b,
    sendHttpReqH :: Request -> StateT Environment m b,
    repeatActionH :: Int -> StateT Environment m b -> StateT Environment m a
  }

-- Keyword search and processing.
handleKeywords ::
  Monad m =>
  WorkHandle m a b ->
  MessageDate ->
  StateT Environment m Command
handleKeywords h@WorkHandle {..} messageDate = do
  env <- get
  let usrName = username $ chat $ message messageDate
      conf = configuration env
  case textM $ message messageDate of
    Just "/repeat" -> do
      _ <- writingLineH INFO $ "Received /repeat from " ++ usrName
      _ <- sendKeyboard h messageDate
      fromServer <- getDataH
      let messageDateLs = case result <$> fromServer of
            Just msgDateLs -> msgDateLs
            _ -> []
      _ <- handleRepeatCommand h messageDate messageDateLs
      pure $ Report "Repeat"
    Just "/help" -> do
      _ <- do
        _ <- writingLineH INFO $ "Received /help from " ++ usrName
        sendComment h messageDate $ mconcat $ messageHelpCommand conf
      pure Help
    _ -> do
      _ <- sendRepeats h messageDate
      pure $ Report "not a keyword"

-- Changing the number of repetitions.
handleRepeatCommand ::
  Monad m =>
  WorkHandle m a b ->
  MessageDate ->
  [MessageDate] ->
  StateT Environment m Command
handleRepeatCommand h@WorkHandle {..} messageDate [] = do
  fromServer <- getDataH
  newMessageDateLs <- case result <$> fromServer of
    Just messageDateLs -> pure messageDateLs
    Nothing ->
      writingLineH ERROR "The array of messages is missing"
        >> pure [MessageDate 0 errorMessage]
  _ <- handleRepeatCommand h messageDate newMessageDateLs
  pure $ Report "empty array"
handleRepeatCommand h@WorkHandle {..} messageDate (x : xs) = do
  env <- get
  let newMessageDate = x
      newEnv = Environment (addNumberH + update_id newMessageDate) (userData env) (configuration env)
      usrNameText = username . chat . message $ messageDate
      usrName = Username usrNameText
      newUsrName = Username . username . chat . message $ newMessageDate
  numRepeats <- case textM (message newMessageDate) >>= readMaybe of
    Just int -> pure int
    Nothing -> writingLineH ERROR "No parse NumRepeats from message" >> pure 0
  if usrName == newUsrName -- We check that the message came from the user
    then --  who requested a change in the number of repetitions.

      if numRepeats `elem` [1 .. 5]
        then do
          _ <-
            sendComment h messageDate $
              "Done! Set up "
                ++ show numRepeats
                ++ " repeat(s)."
          _ <-
            writingLineH INFO $
              "Set up "
                ++ show numRepeats
                ++ " repeat(s) to "
                ++ usrNameText
          put $
            Environment
              (addNumberH + update_id newMessageDate)
              ( Map.insert
                  usrName
                  (NumRepeats numRepeats)
                  (userData env)
              )
              (configuration env)
          pure $ Repeat numRepeats
        else do
          _ <- sendRepeats h newMessageDate
          put newEnv
          pure $ Report "number out of range"
    else do
      _ <- handleKeywords h newMessageDate
      put newEnv
      _ <- handleRepeatCommand h messageDate xs
      pure $ Report "another user"

sendKeyboard :: Monad m => WorkHandle m a b -> MessageDate -> StateT Environment m b
sendKeyboard WorkHandle {..} messageDate = do
  env <- get
  let question = createQuestion messageDate env
      conf = configuration env
      string = stringForCreateKeyboardH messageDate question
      req = createStringRequest conf string
  _ <- writingLineH DEBUG $ show req
  sendHttpReqH req

sendComment :: Monad m => WorkHandle m a b -> MessageDate -> String -> StateT Environment m b
sendComment WorkHandle {..} messageDate msg = do
  conf <- configuration <$> get
  let string = stringCommentH messageDate msg
      req = createStringRequest conf string
  _ <- writingLineH DEBUG $ show req
  sendHttpReqH req
