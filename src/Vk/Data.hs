{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Vk.Data where

import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Object),
    (.:),
  )
import qualified Data.Text as T
import GHC.Generics (Generic)
import Telegram.Data as TG (Media)

-- Data types for VK answer on getLongPollServer request.
data VkKeyServerTs = VkKeyServerTs
  { key :: T.Text,
    server :: T.Text,
    ts :: T.Text
  }
  deriving (Show, Generic, FromJSON)

newtype VkResponse = VkResponse
  {response :: VkKeyServerTs}
  deriving (Show, Generic, FromJSON)

-- Data types for VK answer on
--   BotsLongPollAPI request.
data VkData = VkData
  { offset :: T.Text,
    updates :: [Updates]
  }
  deriving (Show)

instance FromJSON VkData where
  parseJSON (Object v) = do
    offset <- v .: "ts"
    updates <- v .: "updates"
    pure $ VkData offset updates
  parseJSON _ = mempty

data Updates = Updates
  { updates_type :: T.Text,
    updates_object :: ObjectVK
  }
  deriving (Show)

instance FromJSON Updates where
  parseJSON (Object v) = do
    updates_type <- v .: "type"
    updates_object <- v .: "object"
    pure $ Updates updates_type updates_object
  parseJSON _ = mempty

newtype ObjectVK = ObjectVK
  {messageVK :: MessageVK}
  deriving (Show)

instance FromJSON ObjectVK where
  parseJSON (Object v) = do
    messageVK <- v .: "message"
    pure $ ObjectVK messageVK
  parseJSON _ = mempty

data MessageVK = MessageVK
  { from_id :: Int,
    messageVK_id :: Int,
    messageVK_text :: T.Text,
    messageVK_attachments :: [Media]
  }
  deriving (Show)

instance FromJSON MessageVK where
  parseJSON (Object v) = do
    from_id <- v .: "from_id"
    messageVK_id <- v .: "id"
    messageVK_text <- v .: "text"
    messageVK_attachments <- v .: "attachments"
    pure $ MessageVK from_id messageVK_id messageVK_text messageVK_attachments
  parseJSON _ = mempty
