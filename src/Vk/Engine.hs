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
  ( MonadState (get, put),
    MonadTrans (lift),
    StateT,
  )
import Data.Aeson (eitherDecode)
import qualified Data.Text as T
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
getVkData :: Configuration -> T.Text -> T.Text -> T.Text -> IO (Maybe WholeObject)
getVkData conf s k t = do
  x <- connectToServer conf req 0
  writingLine conf DEBUG $ show req
  let obj = eitherDecode $ getResponseBody x
  case obj of
    Left e -> do
      print $ getResponseBody x
      writingLine conf ERROR e
      pure Nothing
    Right v -> do
      writingLine conf DEBUG $ show v
      case updates v of
        [] -> getVkData conf s k $ offset v
        _ -> pure <$> getWholeObjectFromVk conf v
  where
    req =
      parseRequest_ $
        T.unpack $
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
        T.unpack $ apiVKVersion conf
      ]

-- Main program cycle for VK.
botsLongPollAPI :: StateT Environment IO ()
botsLongPollAPI = do
  conf <- configuration <$> get
  x <- lift $ connectToServer conf (getLongPollServerRequest conf) 0
  let code = getResponseStatusCode x
  if code == 200
    then
      ( do
          let obj = eitherDecode $ getResponseBody x
          case obj of
            Left _ -> errorProcessing conf x
            Right v -> do
              let server' = server $ response v
                  key' = key $ response v
                  ts' = ts $ response v
              getAnswer server' key' ts'
      )
    else lift $ writingLine conf ERROR $ "statusCode " ++ show code
  where
    getAnswer s k t = do
      env <- get
      let cnf = configuration env
      getVkDt <- lift $ getVkData cnf s k t
      case getVkDt of
        Nothing -> botsLongPollAPI
        Just w -> do
          let arr = result w
              getVkData' lastUpdId = getVkData cnf s k $ T.pack $ show lastUpdId
              update_id' = if null arr then 0 else (\(x : _) -> update_id x) (reverse arr)
          put $ Environment update_id' (userData env) (configuration env)
          mapM_ (ifKeyWord handler getVkData') arr
          newEnv <- get
          getAnswer s k $ T.pack $ show $ lastUpdate newEnv
    errorProcessing cnf bs = do
      let errObj = eitherDecode $ getResponseBody bs
      case errObj of
        Left err -> lift $ writingLine cnf ERROR $ show err
        Right dta ->
          lift $
            writingLine cnf ERROR $
              mconcat
                [ "Error code ",
                  show $ error_code dta,
                  ". ",
                  T.unpack $ error_msg dta
                ]
