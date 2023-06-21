{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Aeson
  ( FromJSON,
    eitherDecode,
  )
import qualified Data.ByteString.Lazy as L
import GHC.Generics (Generic)
import Logger.Data
  ( Priority,
    logFile,
    time,
  )
import System.Exit (die)

-- Data type for the configuration file.
data Configuration = Configuration
  { messenger :: String,
    hostTG :: String,
    hostVK :: String,
    tokenTG :: String,
    tokenVK :: String,
    groupIdVK :: Int,
    apiVKVersion :: String,
    helpMess :: [String],
    repeatMess :: String,
    defaultRepeats :: Int,
    priorityLevel :: Priority,
    logOutput :: String
  }
  deriving (Show, Generic, FromJSON)

-- Getting information from configuration file.
readConfigFile :: IO Configuration
readConfigFile = do
  t <- time
  content <- L.readFile "config.json"
  case eitherDecode content of
    Right conf -> pure conf
    Left err -> do
      let str = t ++ " UTC   " ++ "ERROR  " ++ " - " ++ err
      print str
      appendFile logFile $ str ++ "\n"
      die "Error reading the configuration file! Check out config.json!"

-- The host of selected messenger.
myHost :: Configuration -> String
myHost conf = do
  case messenger conf of
    "TG" -> hostTG conf
    _ -> hostVK conf

-- The token of selected messenger.
myToken :: Configuration -> String
myToken conf = do
  case messenger conf of
    "TG" -> tokenTG conf
    _ -> tokenVK conf

messengerHost :: Configuration -> String
messengerHost = (++ "/bot") . myHost
