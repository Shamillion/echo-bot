module Vk.Functions where

import Config
  ( Configuration (apiVKVersion, groupIdVK),
    myHost,
    myToken,
  )
import Connect (connectToServer)
import Control.Monad.State.Lazy
  ( StateT,
    get,
    lift,
  )
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Functor.Identity (runIdentity)
import Environment
  ( Environment (lastUpdate),
    UpdateID (..),
    configuration,
  )
import Logger.Data
  ( Priority (DEBUG, ERROR),
  )
import Logger.Functions (writingLine)
import Network.HTTP.Simple
  ( Request,
    Response,
    getResponseBody,
    getResponseStatusCode,
    httpLBS,
    parseRequest_,
  )
import RequestBuilding
  ( createStringRequest,
    stringToUrl,
  )
import System.Exit (die)
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
    VkError (error_code, error_msg),
    VkKeyServerTs (key, server),
    VkResponse (response),
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

-- Function for receiving the first response from the VK server.
getVkResponse :: StateT Environment IO VkResponse
getVkResponse = do
  conf <- configuration <$> get
  resp <- connectToServer (getLongPollServerRequest conf) 0
  let code = getResponseStatusCode resp
  if code == 200
    then do
      let obj = eitherDecode $ getResponseBody resp
      case obj of
        Left _ -> do
          let str = errorProcessing resp
          writingLine ERROR str
          lift $ die str
        Right vkResponse -> pure vkResponse
    else writingLine ERROR ("statusCode " ++ show code) >> getVkResponse
  where
    errorProcessing resp = do
      let errObj = eitherDecode $ getResponseBody resp
      case errObj of
        Left err -> err
        Right vkErr ->
          mconcat
            [ "Error code ",
              show $ error_code vkErr,
              ". ",
              error_msg vkErr
            ]

-- Function for receiving data from the VK server.
getDataVk :: StateT Environment IO (Maybe WholeObject)
getDataVk = do
  vkResponse <- getVkResponse
  env <- get
  let serverVk = server $ response vkResponse
      keyVk = key $ response vkResponse
      tsVk = show $ lastUpdate env
  getAnswer serverVk keyVk tsVk
  where
    getAnswer serverVk keyVk tsVk = do
      let req = parseRequest_ $ mconcat [serverVk, "?act=a_check&key=", keyVk, "&ts=", tsVk, "&wait=25"]
      resp <- connectToServer req 0
      writingLine DEBUG $ show req
      let obj = eitherDecode $ getResponseBody resp
      case obj of
        Left err -> do
          lift $ print $ getResponseBody resp
          writingLine ERROR err
          pure Nothing
        Right vkData -> do
          writingLine DEBUG $ show vkData
          case updates vkData of
            [] -> getAnswer serverVk keyVk $ offset vkData
            _ -> pure <$> getWholeObjectFromVk vkData

getLongPollServerRequest :: Configuration -> Request
getLongPollServerRequest conf =
  parseRequest_ $
    mconcat
      [ "https://",
        myHost conf,
        "/method/groups.getLongPollServer?group_id=",
        show $ groupIdVK conf,
        "&access_token=",
        myToken conf,
        "&v=",
        apiVKVersion conf
      ]
