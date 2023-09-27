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
import Data
  ( Chat (..),
    Media (..),
    Message (..),
    MessageDate (..),
    DataFromServer (..),
  )
import Data.Aeson (eitherDecode, encode)
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
    convertStringToUrl,
  )
import System.Exit (die)
import System.Random (Random (randomRIO))
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
import Vk.KeyboardData (keyboardVk)

-- Functions for converting VK's data to common data.
convertVkToDataFromServer :: VkData -> StateT Environment IO DataFromServer
convertVkToDataFromServer vkData = do
  conf <- configuration <$> get
  updateID <-
    UpdateID <$> case readEither . offset $ vkData of
      Right int -> pure int
      Left err -> writingLine ERROR err >> pure 0
  let messageDateLs = map (convertVkToMessageDate conf updateID) $ updates vkData
  pure $
    DataFromServer
      { ok = True,
        result = messageDateLs
      }

convertVkToMessageDate :: Configuration -> UpdateID -> Updates -> MessageDate
convertVkToMessageDate conf updateID updates =
  MessageDate
    { update_id = updateID,
      message = convertVkToMessage conf updates
    }

convertVkToMessage :: Configuration -> Updates -> Message
convertVkToMessage conf updates =
  Message
    { message_id = messageVK_id $ messageVK $ updates_object updates,
      chat = convertVkToChat conf updates,
      textM = Just $ messageVK_text $ messageVK $ updates_object updates,
      attachments = Just $ messageVK_attachments $ messageVK $ updates_object updates
    }

convertVkToChat :: Configuration -> Updates -> Chat
convertVkToChat conf updates =
  Chat
    { chat_id = groupIdVK conf,
      username = show $ from_id $ messageVK $ updates_object updates
    }

-- Sending a request to VK to return a message.
repeatMessageVk :: MessageDate -> StateT Environment IO (Response LC.ByteString)
repeatMessageVk messageDate = do
  conf <- configuration <$> get
  randInt <- lift (randomRIO (1, 1000000) :: IO Int)
  let userId = username $ chat $ message messageDate
      msg = case textM $ message messageDate of
        Just text -> text
        _ -> ""
      mediaLs = case attachments $ message messageDate of
        Just ls -> ls
        _ -> []
      add = case mediaLs of
        [] -> ""
        _ -> case type_media <$> take 1 mediaLs of
          ["sticker"] -> ""
          _ -> "&attachment="
      string =
        createStringRequest conf $
          mconcat
            [ userId,
              "&random_id=",
              show randInt,
              "&message=",
              convertStringToUrl $ msg ++ add ++ processAttachment mediaLs userId
            ]
  writingLine DEBUG $ show string
  httpLBS string

stringForCreateKeyboard :: MessageDate -> String -> String
stringForCreateKeyboard messageDate question =
  mconcat
    [ username $ chat $ message messageDate,
      "&random_id=0",
      "&message=",
      convertStringToUrl question,
      "&keyboard=",
      convertStringToUrl $ LC.unpack $ encode keyboardVk
    ]

stringComment :: MessageDate -> String -> String
stringComment messageDate msg =
  mconcat
    [ username $ chat $ message messageDate,
      "&random_id=0",
      "&message=",
      convertStringToUrl msg
    ]

processAttachment :: [Media] -> String -> String
processAttachment [] _ = ""
processAttachment (x : xs) userId = case x of
  Sticker _ stickerId -> "&sticker_id=" ++ show stickerId ++ processAttachment xs userId
  AudioMessage _ lnk -> lnk ++ processAttachment xs userId
  Others typeMedia mediaId ownerId urlMedia accessKey -> runIdentity $ do
    let lnk = case urlMedia of
          Just txt -> txt
          _ -> ""
    pure $
      if typeMedia == "doc" && userId == show ownerId
        then lnk ++ "," ++ processAttachment xs userId
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
              processAttachment xs userId
            ]

-- Function for receiving the first response from the VK server.
getVkResponse :: StateT Environment IO VkResponse
getVkResponse = do
  conf <- configuration <$> get
  resp <- connectToServer (getLongPollServerRequest conf) 0
  let code = getResponseStatusCode resp
  if code == 200
    then do
      let eitherVkResponse = eitherDecode $ getResponseBody resp
      case eitherVkResponse of
        Left _ -> do
          let err = errorProcessing resp
          writingLine ERROR err
          lift $ die err
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
getDataVk :: StateT Environment IO (Maybe DataFromServer)
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
      let eitherVkData = eitherDecode $ getResponseBody resp
      case eitherVkData of
        Left err -> do
          lift $ print $ getResponseBody resp
          writingLine ERROR err
          pure Nothing
        Right vkData -> do
          writingLine DEBUG $ show vkData
          case updates vkData of
            [] -> getAnswer serverVk keyVk $ offset vkData
            _ -> pure <$> convertVkToDataFromServer vkData

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
