{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import GHC.Generics (Generic)
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
configuration :: IO Configuration
configuration = do
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
