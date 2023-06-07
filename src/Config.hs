{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad.State.Lazy (when)
import Data.Aeson
  ( FromJSON,
    eitherDecode,
  )
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import GHC.Generics (Generic)
import Network.HTTP.Simple
  ( HttpException,
    Request,
    Response,
    httpLBS,
  )
import System.Exit (die)

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

-- Getting current time for the logger.
time :: IO String
time = take 19 . show <$> getCurrentTime

-- Name of the logfile.
logFile :: String
logFile = "log.log"

-- Getting information from configuration file.
getConfiguration :: IO Configuration
getConfiguration = do
  t <- time
  content <- L.readFile "config.json"
  case eitherDecode content of
    Right conf -> pure conf
    Left err -> do
      let str = t ++ " UTC   " ++ "ERROR  " ++ " - " ++ err
      print str
      appendFile logFile $ str ++ "\n"
      die "Error reading the configuration file! Check out config.json!"

-- Selected messenger.
currentMessenger :: IO T.Text
currentMessenger = messenger <$> getConfiguration

-- The host of selected messenger.
myHost :: IO String
myHost = do
  conf <- getConfiguration
  crntMsngr <- currentMessenger
  pure $ case crntMsngr of
    "TG" -> T.unpack $ hostTG conf
    _ -> T.unpack $ hostVK conf

-- The token of selected messenger.
myToken :: IO String
myToken = do
  conf <- getConfiguration
  crntMsngr <- currentMessenger
  pure $ case crntMsngr of
    "TG" -> T.unpack $ tokenTG conf
    _ -> T.unpack $ tokenVK conf

messengerHost :: IO String
messengerHost = (++ "/bot") <$> myHost

-- Logging level.
logLevel :: IO Priority
logLevel = priorityLevel <$> getConfiguration

-- Function writes information to log.
writingLine :: Priority -> String -> IO ()
writingLine lvl str = do
  logLevel' <- logLevel
  if lvl >= logLevel'
    then do
      t <- time
      let string = t ++ " UTC   " ++ showLevel lvl ++ " - " ++ str
      out <- logOutput <$> getConfiguration
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

-- Function for connecting to the server.
connectToServer :: Request -> Int -> IO (Response LC.ByteString)
connectToServer req num = do
  x <- try $ httpLBS req
  writingLine DEBUG $ show req
  case x of
    Left e -> do
      writingLine ERROR $ show (e :: HttpException)
      when (num == 0) $ do
        getCurrentTime >>= print
        putStrLn "Connection Failure"
        putStrLn "Trying to set a connection... "
      threadDelay 1000000
      connectToServer req (num + 1)
    Right v -> do
      writingLine DEBUG $ show v
      when (num /= 0) $ do
        getCurrentTime >>= print
        putStrLn "Connection restored"
      pure v
