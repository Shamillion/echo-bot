{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module KeyboardData where

import Data.Aeson
  ( ToJSON (toJSON),
    object,
    (.=),
  )
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

-- Data types for the VK keyboard.
data ActionVk = ActionVk
  { typeActionVk :: T.Text,
    label :: T.Text
  }
  deriving (Show)

instance ToJSON ActionVk where
  toJSON (ActionVk typeActionVk label) =
    object
      [ "type" .= typeActionVk,
        "label" .= label
      ]

newtype ButtonVk = ButtonVk
  {action :: ActionVk}
  deriving (Show, Generic, ToJSON)

data KeyboardVk = KeyboardVk
  { one_time :: Bool,
    buttonsVk :: [[ButtonVk]]
  }
  deriving (Show)

instance ToJSON KeyboardVk where
  toJSON (KeyboardVk one_time buttonsVk) =
    object
      [ "one_time" .= one_time,
        "buttons" .= buttonsVk
      ]

-- Functions types for the VK keyboard.
buttonVk :: Int -> ButtonVk
buttonVk num =
  ButtonVk
    { action = toAction num
    }

toAction :: Int -> ActionVk
toAction num =
  ActionVk
    { typeActionVk = "text",
      label = T.pack $ show num
    }

keyboardVk :: KeyboardVk
keyboardVk =
  KeyboardVk
    { one_time = True,
      buttonsVk = [map buttonVk [1 .. 5]]
    }
