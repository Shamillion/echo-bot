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

data Configuration = Configuration
  { messenger :: String,
    hostTG :: String,
    hostVK :: String,
    tokenTG :: String,
    tokenVK :: String,
    groupIdVK :: Int,
    apiVKVersion :: String,
    messageHelpCommand :: [String],
    messageRepeatCommand :: String,
    defaultRepeats :: Int,
    priorityLevel :: Priority,
    logOutput :: String
  }
  deriving (Show, Generic, FromJSON)

readConfigFile :: IO Configuration
readConfigFile = do
  time' <- time
  content <- L.readFile "config.json"
  case eitherDecode content of
    Right conf -> pure conf
    Left err -> do
      let str = time' ++ " UTC   " ++ "ERROR  " ++ " - " ++ err
      print str
      appendFile logFile $ str ++ "\n"
      die "Error reading the configuration file! Check out config.json!"

myHost :: Configuration -> String
myHost conf = do
  case messenger conf of
    "TG" -> hostTG conf
    _ -> hostVK conf

myToken :: Configuration -> String
myToken conf = do
  case messenger conf of
    "TG" -> tokenTG conf
    _ -> tokenVK conf

messengerHost :: Configuration -> String
messengerHost = (++ "/bot") . myHost
