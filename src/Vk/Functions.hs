module Vk.Functions where

import Config
  ( Configuration (groupIdVK),
  )
import Control.Monad.State.Lazy
  ( StateT,
    get,
    lift,
  )
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Functor.Identity (runIdentity)
import Environment
  ( Environment,
    UpdateID (..),
    configuration,
  )
import Logger.Data
  ( Priority (DEBUG, ERROR),
  )
import Logger.Functions (writingLine)
import Network.HTTP.Simple
  ( Response,
    httpLBS,
  )
import RequestBuilding
  ( createStringRequest,
    stringToUrl,
  )
import System.Random (Random (randomRIO))
import Telegram.Data as TG
  ( Chat (..),
    Media (..),
    Message (..),
    MessageDate (..),
    WholeObject (..),
  )
import Text.Read (readEither)
import Vk.Data
  ( MessageVK
      ( from_id,
        messageVK_attachments,
        messageVK_id,
        messageVK_text
      ),
    ObjectVK (messageVK),
    Updates (updates_object),
    VkData (offset, updates),
  )

-- Functions for converting VK's data to Telegrams's data.
getWholeObjectFromVk :: VkData -> StateT Environment IO WholeObject
getWholeObjectFromVk obj = do
  conf <- configuration <$> get
  num <-
    UpdateID <$> case readEither . offset $ obj of
      Right int -> pure int
      Left err -> writingLine ERROR err >> pure 0
  let ls = map (getMessageDateFromVk conf num) $ updates obj
  pure $
    WholeObject
      { ok = True,
        result = ls
      }

getMessageDateFromVk :: Configuration -> UpdateID -> Updates -> MessageDate
getMessageDateFromVk conf num obj =
  MessageDate
    { update_id = num,
      TG.message = getMessageFromVk conf obj
    }

getMessageFromVk :: Configuration -> Updates -> Message
getMessageFromVk conf obj =
  Message
    { message_id = messageVK_id $ messageVK $ updates_object obj,
      chat = getChatFromVk conf obj,
      textM = Just $ messageVK_text $ messageVK $ updates_object obj,
      TG.attachments = Just $ messageVK_attachments $ messageVK $ updates_object obj
    }

getChatFromVk :: Configuration -> Updates -> Chat
getChatFromVk conf obj =
  Chat
    { chat_id = groupIdVK conf,
      username = show $ from_id $ messageVK $ updates_object obj
    }

-- Sending a request to VK to return a message.
repeatMessageVk :: MessageDate -> StateT Environment IO (Response LC.ByteString)
repeatMessageVk obj = do
  conf <- configuration <$> get
  randInt <- lift (randomRIO (1, 1000000) :: IO Int)
  let userId = username $ chat $ message obj
      str = case textM $ message obj of
        Just messText -> messText
        _ -> ""
      arr = case attachments $ message obj of
        Just ls -> ls
        _ -> []
      add = case arr of
        [] -> ""
        _ -> case type_media $ (\(x : _) -> x) arr of
          "sticker" -> ""
          _ -> "&attachment="
      string =
        createStringRequest conf $
          mconcat
            [ userId,
              "&random_id=",
              show randInt,
              "&message=",
              stringToUrl $ str ++ add ++ attachment arr userId
            ]
  writingLine DEBUG $ show string
  httpLBS string

-- Processing of attachments for VK.
attachment :: [Media] -> String -> String
attachment [] _ = ""
attachment (x : xs) userId = case x of
  Sticker _ stickerId -> "&sticker_id=" ++ show stickerId ++ attachment xs userId
  AudioMessage _ lnk -> lnk ++ attachment xs userId
  Others typeMedia mediaId ownerId urlMedia accessKey -> runIdentity $ do
    let lnk = case urlMedia of
          Just txt -> txt
          _ -> ""
    pure $
      if typeMedia == "doc" && userId == show ownerId
        then lnk ++ "," ++ attachment xs userId
        else
          mconcat
            [ typeMedia,
              show ownerId,
              "_",
              show mediaId,
              case accessKey of
                Just str -> "_" ++ str
                _ -> "",
              ",",
              attachment xs userId
            ]
