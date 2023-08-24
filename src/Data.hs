{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data where

import Control.Applicative (Alternative ((<|>)))
import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Object),
    withObject,
    (.!=),
    (.:),
    (.:?),
  )
import Data.Foldable (asum)
import Environment
  ( UpdateID (UpdateID),
  )
import GHC.Generics (Generic)

-- Data types for the answer.
data Chat = Chat
  { chat_id :: Int,
    username :: String
  }
  deriving (Show)

instance FromJSON Chat where
  parseJSON (Object v) = do
    chat_id <- v .: "id"
    username <- v .: "username"
    pure $ Chat chat_id username
  parseJSON _ = mempty

errorChat :: Chat
errorChat = Chat 0 "error"

data Message = Message
  { message_id :: Int,
    chat :: Chat,
    textM :: Maybe String,
    attachments :: Maybe [Media]
  }
  deriving (Show)

instance FromJSON Message where
  parseJSON (Object v) = do
    message_id <- v .: "message_id"
    chat <- v .: "chat"
    textM <- v .:? "text"
    attachments <- v .:? "attachments"
    pure $ Message message_id chat textM attachments
  parseJSON _ = mempty

errorMessage :: Message
errorMessage =
  Message
    { message_id = 0,
      chat = errorChat,
      textM = Nothing,
      attachments = Nothing
    }

data MessageDate = MessageDate
  { update_id :: UpdateID,
    message :: Message
  }
  deriving (Show)

instance FromJSON MessageDate where
  parseJSON (Object v) = do
    num <- v .: "update_id"
    message <- v .:? "message" .!= errorMessage
    pure $ MessageDate (UpdateID num) message
  parseJSON _ = mempty

data DataFromServer = DataFromServer
  { ok :: Bool,
    result :: [MessageDate]
  }
  deriving (Show, Generic, FromJSON)

data Media
  = Sticker
      { type_media :: String,
        sticker_id :: Int
      }
  | AudioMessage
      { type_media :: String,
        link_mp3 :: String
      }
  | Others
      { type_media :: String,
        media_id :: Int,
        owner_id :: Int,
        url :: Maybe String,
        access_key :: Maybe String
      }
  deriving (Show)

instance FromJSON Media where
  parseJSON = withObject "Media" $ \v ->
    asum
      [ do
          type_media <- v .: "type"
          obj <- v .: "sticker"
          sticker_id <- obj .: "sticker_id"
          pure $ Sticker type_media sticker_id,
        do
          type_media <- v .: "type"
          obj <- v .: "audio_message"
          link_mp3 <- obj .: "link_mp3"
          pure $ AudioMessage type_media link_mp3,
        do
          type_media <- v .: "type"
          obj <-
            v .: "photo" <|> v .: "video" <|> v .: "audio" <|> v .: "doc"
              <|> v .: "market"
              <|> v .: "poll"
              <|> v .: "wall"
          media_id <- obj .: "id"
          owner_id <- obj .: "owner_id"
          url <- obj .:? "url"
          access_key <- obj .:? "access_key"
          pure $ Others type_media media_id owner_id url access_key
      ]
