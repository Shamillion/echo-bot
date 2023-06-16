{-# LANGUAGE DerivingVia #-}

module Environment where

import Config
  ( Configuration (defaultRepeats, hostTG, hostVK, messenger, tokenTG, tokenVK),
    readConfigFile,
  )
import qualified Data.Map.Lazy as Map
import Data.String (IsString)
import qualified Data.Text as T

newtype UpdateID = UpdateID Int
  deriving (Eq)
  deriving (Num) via Int

instance Show UpdateID where
  show (UpdateID a) = show a

newtype Username = Username T.Text
  deriving (Eq, Ord)
  deriving (IsString) via T.Text

newtype NumRepeats = NumRepeats Int
  deriving (Eq, Show)

data Environment = Environment
  { lastUpdate :: UpdateID,
    userData :: Map.Map Username NumRepeats,
    configuration :: Configuration
  }

-- environment :: IO Environment
-- environment =
--   (Environment 0 . Map.singleton "" . NumRepeats)
--     . defaultRepeats
--     <$> getConfiguration

environment :: IO Environment
environment = do
  conf <- readConfigFile
  pure $
    Environment
      { lastUpdate = 0,
        userData = Map.singleton "" . NumRepeats . defaultRepeats $ conf,
        configuration = conf
      }

getConfiguration :: IO Configuration
getConfiguration = configuration <$> environment

-- Selected messenger.
currentMessenger :: IO T.Text
currentMessenger = messenger <$> getConfiguration

-- The host of selected messenger.
myHost :: Configuration -> String
myHost conf = do
  let crntMsngr = messenger conf
  case crntMsngr of
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

messengerHost :: Configuration -> String
messengerHost = (++ "/bot") . myHost
