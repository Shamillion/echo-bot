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
import Environment
  ( Environment (..),
  )
import Lib
  ( handler,
    ifKeyWord,
  )
import Logger.Data (Priority (DEBUG, ERROR))
import Logger.Functions (writingLine)
import Network.HTTP.Simple
  ( Request,
    getResponseBody,
    getResponseStatusCode,
    parseRequest_,
  )
import Telegram.Data as TG
  ( MessageDate (..),
    WholeObject (..),
  )
import Vk.Data
  ( VkData (..),
    VkError (..),
    VkKeyServerTs (..),
    VkResponse (response),
  )
import Vk.Functions (getWholeObjectFromVk)

-- Function for receiving data from the VK server.
getVkData :: String -> String -> String -> StateT Environment IO (Maybe WholeObject)
getVkData serverVk keyVk tsVk = do
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
        [] -> getVkData serverVk keyVk $ offset vkData
        _ -> pure <$> getWholeObjectFromVk vkData
  where
    req =
      parseRequest_ $
        mconcat [serverVk, "?act=a_check&key=", keyVk, "&ts=", tsVk, "&wait=25"]

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
  conf <- configuration <$> get
  resp <- connectToServer (getLongPollServerRequest conf) 0
  let code = getResponseStatusCode resp
  if code == 200
    then
      ( do
          let obj = eitherDecode $ getResponseBody resp
          case obj of
            Left _ -> errorProcessing resp
            Right v -> do
              let server' = server $ response v
                  key' = key $ response v
                  ts' = ts $ response v
              getAnswer server' key' ts'
      )
    else writingLine ERROR $ "statusCode " ++ show code
  where
    getAnswer serverVk keyVk tsVk = do
      env <- get
      getVkDt <- getVkData serverVk keyVk tsVk
      case getVkDt of
        Nothing -> botsLongPollAPI
        Just wholeObj -> do
          let arr = result wholeObj
              getVkData' lastUpdId = getVkData serverVk keyVk $ show lastUpdId
              update_id' = if null arr then 0 else (\(x : _) -> update_id x) (reverse arr)
          put $ Environment update_id' (userData env) (configuration env)
          mapM_ (ifKeyWord handler getVkData') arr
          newEnv <- get
          getAnswer serverVk keyVk $ show $ lastUpdate newEnv
    errorProcessing resp = do
      let errObj = eitherDecode $ getResponseBody resp
      case errObj of
        Left err -> writingLine ERROR $ show err
        Right vkErr ->
          writingLine ERROR $
            mconcat
              [ "Error code ",
                show $ error_code vkErr,
                ". ",
                error_msg vkErr
              ]
