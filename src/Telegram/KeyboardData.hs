{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Telegram.KeyboardData where

import Data.Aeson (ToJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- Data type for the Telegram keyboard.
newtype KeyboardButton = KeyboardButton
  {text :: T.Text}
  deriving (Show, Generic, ToJSON)

data ReplyKeyboardMarkup = ReplyKeyboardMarkup
  { keyboard :: [[KeyboardButton]],
    resize_keyboard :: Bool,
    one_time_keyboard :: Bool
  }
  deriving (Show, Generic, ToJSON)

-- Creating keyboard for TG.
toKeyboardButton :: Int -> KeyboardButton
toKeyboardButton num = KeyboardButton {text = T.pack $ show num}

createKeyboard :: ReplyKeyboardMarkup
createKeyboard =
  ReplyKeyboardMarkup
    { keyboard = [map toKeyboardButton [1 .. 5]],
      resize_keyboard = True,
      one_time_keyboard = True
    }
