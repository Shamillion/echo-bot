{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Telegram.KeyboardData where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

-- Data type for the Telegram keyboard.
newtype KeyboardButton = KeyboardButton
  {text :: String}
  deriving (Show, Generic, ToJSON)

data ReplyKeyboardMarkup = ReplyKeyboardMarkup
  { keyboard :: [[KeyboardButton]],
    resize_keyboard :: Bool,
    one_time_keyboard :: Bool
  }
  deriving (Show, Generic, ToJSON)

-- Creating keyboard for TG.
createKeyboardButton :: Int -> KeyboardButton
createKeyboardButton num = KeyboardButton {text = show num}

createKeyboard :: ReplyKeyboardMarkup
createKeyboard =
  ReplyKeyboardMarkup
    { keyboard = [map createKeyboardButton [1 .. 5]],
      resize_keyboard = True,
      one_time_keyboard = True
    }
