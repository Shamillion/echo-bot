{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Telegram where

import Config
  ( Priority (ERROR),
    connectToServer,
    messengerHost,
    myToken,
    writingLine,
  )
import Control.Applicative (Alternative ((<|>)))
import Control.Monad.State.Lazy
  ( MonadState (get, put),
    MonadTrans (lift),
    StateT,
  )
import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Object),
    decode,
    withObject,
    (.!=),
    (.:),
    (.:?),
  )
import Data.Foldable (asum)
import qualified Data.Text as T
import Environment
  ( Environment (Environment, lastUpdate, userData),
    UpdateID (UpdateID),
  )
import GHC.Generics (Generic)
import Network.HTTP.Simple
  ( getResponseBody,
    getResponseStatusCode,
    parseRequest_,
  )

-- Data types for the Telegram answer.
data Chat = Chat
  { chat_id :: Int,
    username :: T.Text
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
    textM :: Maybe T.Text,
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

data WholeObject = WholeObject
  { ok :: Bool,
    result :: [MessageDate]
  }
  deriving (Show, Generic, FromJSON)

data Media
  = Sticker
      { type_media :: T.Text,
        sticker_id :: Int
      }
  | AudioMessage
      { type_media :: T.Text,
        link_mp3 :: T.Text
      }
  | Others
      { type_media :: T.Text,
        media_id :: Int,
        owner_id :: Int,
        url :: Maybe T.Text,
        access_key :: Maybe T.Text
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

-- Update request string for Telegram.
createStringGetUpdates :: UpdateID -> String
createStringGetUpdates (UpdateID num) =
  mconcat ["/getUpdates?offset=", show num, "&timeout=1"]

-- Function for getting data from Telegram's server.
getData :: StateT Environment IO (Maybe WholeObject)
getData = do
  env <- get
  let str = pure . createStringGetUpdates . lastUpdate $ env
  req <-
    lift $
      parseRequest_
        <$> mconcat [pure "https://", messengerHost, myToken, str]
  x <- lift $ connectToServer req 0
  let code = getResponseStatusCode x
  if code == 200
    then
      ( do
          let obj = decode $ getResponseBody x
          case result <$> obj of
            Just [] -> do
              put $ Environment 1 (userData env)
              getData
            _ -> do
              pure obj
      )
    else lift $ do
      writingLine ERROR $ "statusCode " ++ show code
      pure Nothing
