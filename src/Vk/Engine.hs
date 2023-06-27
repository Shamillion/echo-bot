module Vk.Engine where

import Config
  ( Configuration
      ( apiVKVersion,
        groupIdVK
      ),
    myHost,
    myToken,
  )
import Connect (connectToServer)
import Control.Monad.State.Lazy
  ( StateT,
    get,
    lift,
    put,
  )
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as LC
import Environment
  ( Environment (..),
    UpdateID (UpdateID),
  )
import Lib
  ( WorkHandle (..),
    handlerTg,
    ifKeyWord,
  )
import Logger.Data (Priority (DEBUG, ERROR))
import Logger.Functions (writingLine)
import Network.HTTP.Simple
  ( Request,
    Response,
    getResponseBody,
    getResponseStatusCode,
    parseRequest_,
  )
import System.Exit (die)
import Telegram.Data as TG
  ( MessageDate (..),
    WholeObject (..),
  )
import Text.Read (readEither)
import Vk.Data
  ( VkData (..),
    VkError (..),
    VkKeyServerTs (..),
    VkResponse (response),
  )
import Vk.Functions (getWholeObjectFromVk)

-- Handle for work of echobot.
handlerVk :: WorkHandle IO () (Response LC.ByteString)
handlerVk =
  handlerTg
    { getDataH = getVkData
    }

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
getVkData :: StateT Environment IO (Maybe WholeObject)
getVkData = do
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

-- Main program cycle for VK.
botsLongPollAPI :: StateT Environment IO ()
botsLongPollAPI = do
  env <- get
  if lastUpdate env == 0
    then do
      resp <- getVkResponse
      num <-
        UpdateID <$> case readEither . ts . response $ resp of
          Right int -> pure int
          Left err -> writingLine ERROR err >> pure 0
      --  put $ Environment num (userData env) (configuration env)
      put $ env {lastUpdate = num}
      botsLongPollAPI
    else do
      getVkDt <- getVkData
      case getVkDt of
        Nothing -> botsLongPollAPI
        Just wholeObj -> do
          let arr = result wholeObj
              update_id' = if null arr then 0 else (\(x : _) -> update_id x) (reverse arr)
          -- put $ Environment update_id' (userData env) (configuration env)
          put $ env {lastUpdate = update_id'}
          mapM_ (ifKeyWord handlerVk) arr
          botsLongPollAPI
