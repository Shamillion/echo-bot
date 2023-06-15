{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Aeson
  ( FromJSON,
    eitherDecode,
  )
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import GHC.Generics (Generic)
import Logger.Data
  ( Priority,
    logFile,
    time,
  )
import System.Exit (die)

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