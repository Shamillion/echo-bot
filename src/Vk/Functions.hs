module Vk.Functions where

import Config
  ( Configuration (groupIdVK),
  )
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Functor.Identity (runIdentity)
import qualified Data.Text as T
import Environment
  ( UpdateID (..),
    getConfiguration,
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
getWholeObjectFromVk :: VkData -> IO WholeObject
getWholeObjectFromVk obj = do
  num <-
    UpdateID <$> case readEither . T.unpack . offset $ obj of
      Right n -> pure n
      Left e -> writingLine ERROR e >> pure 0
  ls <- mapM (getMessageDateFromVk num) $ updates obj
  pure $
    WholeObject
      { ok = True,
        result = ls
      }

getMessageDateFromVk :: UpdateID -> Updates -> IO MessageDate
getMessageDateFromVk num obj = do
  getMessageFromVk' <- getMessageFromVk obj
  pure $
    MessageDate
      { update_id = num,
        TG.message = getMessageFromVk'
      }

getMessageFromVk :: Updates -> IO Message
getMessageFromVk obj = do
  getChatFromVk' <- getChatFromVk obj
  pure $
    Message
      { message_id = messageVK_id $ messageVK $ updates_object obj,
        chat = getChatFromVk',
        textM = Just $ messageVK_text $ messageVK $ updates_object obj,
        TG.attachments = Just $ messageVK_attachments $ messageVK $ updates_object obj
      }

getChatFromVk :: Updates -> IO Chat
getChatFromVk obj = do
  conf <- getConfiguration
  pure $
    Chat
      { chat_id = groupIdVK conf,
        username = T.pack $ show $ from_id $ messageVK $ updates_object obj
      }

-- Sending a request to VK to return a message.
repeatMessageVk :: MessageDate -> IO (Response LC.ByteString)
repeatMessageVk obj = do
  r <- randomRIO (1, 1000000) :: IO Int
  let userId = T.unpack $ username $ chat $ message obj
      str = case textM $ message obj of
        Just s -> s
        _ -> ""
      arr = case attachments $ message obj of
        Just ls -> ls
        _ -> []
      add = case arr of
        [] -> ""
        _ -> case type_media $ (\(x : _) -> x) arr of
          "sticker" -> ""
          _ -> "&attachment="
  string <-
    createStringRequest $
      mconcat
        [ userId,
          "&random_id=",
          show r,
          "&message=",
          stringToUrl $ T.unpack str ++ add ++ attachment arr userId
        ]
  writingLine DEBUG $ show string
  httpLBS string

-- Processing of attachments for VK.
attachment :: [Media] -> String -> String
attachment [] _ = ""
attachment (x : xs) userId = case x of
  Sticker _ n -> "&sticker_id=" ++ show n ++ attachment xs userId
  AudioMessage _ l -> T.unpack l ++ attachment xs userId
  Others t mI oI u k -> runIdentity $ do
    let lnk = case u of
          Just txt -> txt
          _ -> ""
    pure $
      if t == "doc" && userId == show oI
        then T.unpack lnk ++ "," ++ attachment xs userId
        else
          mconcat
            [ T.unpack t,
              show oI,
              "_",
              show mI,
              case k of
                Just s -> "_" ++ T.unpack s
                _ -> "",
              ",",
              attachment xs userId
            ]
