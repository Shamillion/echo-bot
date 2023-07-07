{-# LANGUAGE DerivingVia #-}

module Lib where

import Config
  ( Configuration
      ( helpMess
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
    WholeObject (result),
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

-- Sending repetitions of request.
sendRepeats :: Monad m => WorkHandle m a b -> MessageDate -> StateT Environment m a
sendRepeats WorkHandle {..} obj = do
  env <- get
  let NumRepeats num = getNumRepeats obj env -- Getting the number of repeats for a current user.                                --    a message.
  repeatActionH num $ repeatMessageH obj -- Repeat the action num times.

data Command = Repeat Int | Help | Report String
  deriving (Eq, Show)

-- Handle Pattern
data WorkHandle m a b = WorkHandle
  { writingLineH :: Priority -> String -> StateT Environment m a,
    getDataH :: StateT Environment m (Maybe WholeObject),
    addNumberH :: UpdateID,
    stringForCreateKeyboardH :: MessageDate -> String -> String,
    stringCommentH :: MessageDate -> String -> String,
    repeatMessageH :: MessageDate -> StateT Environment m b,
    sendHttpReqH :: Request -> StateT Environment m b,
    repeatActionH :: Int -> StateT Environment m b -> StateT Environment m a
  }

-- Keyword search and processing.
ifKeyWord ::
  Monad m =>
  WorkHandle m a b ->
  MessageDate ->
  StateT Environment m Command
ifKeyWord h@WorkHandle {..} obj = do
  env <- get
  let usrName = username $ chat $ message obj
      conf = configuration env
  case textM $ message obj of
    Just "/repeat" -> do
      _ <- writingLineH INFO $ "Received /repeat from " ++ usrName
      _ <- sendKeyboard h obj
      fromServer <- getDataH
      let arr = case result <$> fromServer of
            Just messageDateLs -> messageDateLs
            _ -> []
      wordIsRepeat h obj arr
    Just "/help" -> do
      _ <- do
        _ <- writingLineH INFO $ "Received /help from " ++ usrName
        sendComment h obj $ mconcat $ helpMess conf
      pure Help
    _ -> do
      _ <- sendRepeats h obj
      pure $ Report "not a keyword"

-- Changing the number of repetitions.
wordIsRepeat ::
  Monad m =>
  WorkHandle m a b ->
  MessageDate ->
  [MessageDate] ->
  StateT Environment m Command
wordIsRepeat h@WorkHandle {..} obj [] = do
  fromServer <- getDataH
  newArr <- case result <$> fromServer of
    Just messageDateLs -> pure messageDateLs
    Nothing ->
      writingLineH ERROR "The array of messages is missing"
        >> pure [MessageDate 0 errorMessage]
  wordIsRepeat h obj newArr
wordIsRepeat h@WorkHandle {..} obj (x : xs) = do
  env <- get
  let newObj = x
      newEnv = Environment (addNumberH + update_id newObj) (userData env) (configuration env)
      usrNameText = username . chat . message $ obj
      usrName = Username usrNameText
      newUsrName = Username . username . chat . message $ newObj
  numRepeats <- case textM (message newObj) >>= readMaybe of
    Just int -> pure int
    Nothing -> writingLineH ERROR "No parse NumRepeats from message" >> pure 0
  if usrName == newUsrName -- We check that the message came from the user
    then --  who requested a change in the number of repetitions.

      if numRepeats `elem` [1 .. 5]
        then do
          _ <-
            sendComment h obj $
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
              (addNumberH + update_id newObj)
              ( Map.insert
                  usrName
                  (NumRepeats numRepeats)
                  (userData env)
              )
              (configuration env)
          pure $ Repeat numRepeats
        else do
          _ <- sendRepeats h newObj
          put newEnv
          pure $ Report "number out of range"
    else do
      _ <- ifKeyWord h newObj
      put newEnv
      wordIsRepeat h obj xs

sendKeyboard :: Monad m => WorkHandle m a b -> MessageDate -> StateT Environment m b
sendKeyboard WorkHandle {..} obj = do
  env <- get
  let question = createQuestion obj env
      conf = configuration env
      string = stringForCreateKeyboardH obj question
      req = createStringRequest conf string
  _ <- writingLineH DEBUG $ show req
  sendHttpReqH req

sendComment :: Monad m => WorkHandle m a b -> MessageDate -> String -> StateT Environment m b
sendComment WorkHandle {..} obj str = do
  conf <- configuration <$> get
  let string = stringCommentH obj str
      req = createStringRequest conf string
  _ <- writingLineH DEBUG $ show req
  sendHttpReqH req
