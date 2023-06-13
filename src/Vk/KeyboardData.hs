{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Vk.KeyboardData where

import Data.Aeson
  ( ToJSON (toJSON),
    object,
    (.=),
  )
import qualified Data.Text as T
import GHC.Generics (Generic)

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
