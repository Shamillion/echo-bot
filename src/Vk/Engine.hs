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
  ( get, 
    put,
    lift,
    StateT,
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
getVkData s k t = do
  x <- connectToServer req 0
  writingLine DEBUG $ show req
  let obj = eitherDecode $ getResponseBody x
  case obj of
    Left e -> do
      lift $ print $ getResponseBody x
      writingLine ERROR e
      pure Nothing
    Right v -> do
      writingLine DEBUG $ show v
      case updates v of
        [] -> getVkData s k $ offset v
        _ -> pure <$> getWholeObjectFromVk v
  where
    req =
      parseRequest_ $
        mconcat [s, "?act=a_check&key=", k, "&ts=", t, "&wait=25"]

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
  x <- connectToServer (getLongPollServerRequest conf) 0
  let code = getResponseStatusCode x
  if code == 200
    then
      ( do
          let obj = eitherDecode $ getResponseBody x
          case obj of
            Left _ -> errorProcessing x
            Right v -> do
              let server' = server $ response v
                  key' = key $ response v
                  ts' = ts $ response v
              getAnswer server' key' ts'
      )
    else writingLine ERROR $ "statusCode " ++ show code
  where
    getAnswer s k t = do
      env <- get
      getVkDt <- getVkData s k t
      case getVkDt of
        Nothing -> botsLongPollAPI
        Just w -> do
          let arr = result w
              getVkData' lastUpdId = getVkData s k $ show lastUpdId
              update_id' = if null arr then 0 else (\(x : _) -> update_id x) (reverse arr)
          put $ Environment update_id' (userData env) (configuration env)
          mapM_ (ifKeyWord handler getVkData') arr
          newEnv <- get
          getAnswer s k $ show $ lastUpdate newEnv
    errorProcessing bs = do
      let errObj = eitherDecode $ getResponseBody bs
      case errObj of
        Left err -> writingLine ERROR $ show err
        Right dta ->
            writingLine ERROR $
              mconcat
                [ "Error code ",
                  show $ error_code dta,
                  ". ",
                  error_msg dta
                ]
