module Telegram.Functions where

import Config
  ( messengerHost,
    myToken,
  )
import Connect (connectToServer)
import Control.Monad.State.Lazy
  ( StateT,
    get,
    lift,
    put,
  )
import Data
  ( Chat (chat_id),
    Message (chat),
    MessageDate (message, update_id),
    WholeObject (result),
    chat,
    chat_id,
    message,
    message_id,
  )
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Time (getCurrentTime)
import Environment
  ( Environment
      ( Environment,
        configuration,
        lastUpdate,
        userData
      ),
    UpdateID (UpdateID),
  )
import Logger.Data (Priority (DEBUG, ERROR))
import Logger.Functions (writingLine)
import Network.HTTP.Simple
  ( Response,
    getResponseBody,
    getResponseStatusCode,
    httpLBS,
    parseRequest_,
  )
import RequestBuilding (createStringRequest, stringToUrl)
import Telegram.KeyboardData (createKeyboard)

-- Update request string for Telegram.
createStringGetUpdates :: UpdateID -> String
createStringGetUpdates (UpdateID num) =
  mconcat ["/getUpdates?offset=", show num, "&timeout=1"]

-- Function for getting data from Telegram's server.
getDataTg :: StateT Environment IO (Maybe WholeObject)
getDataTg = do
  env <- get
  let str = createStringGetUpdates . lastUpdate $ env
      conf = configuration env
      req =
        parseRequest_ $
          mconcat ["https://", messengerHost conf, myToken conf, str]
  resp <- connectToServer req 0
  let code = getResponseStatusCode resp
  if code == 200
    then
      ( do
          let obj = decode $ getResponseBody resp
          case result <$> obj of
            Just [] -> do
              put $ Environment 1 (userData env) (configuration env)
              getDataTg
            _ -> do
              pure obj
      )
    else do
      writingLine ERROR $ "statusCode " ++ show code
      pure Nothing

-- Function for getting update_id  for the first time.
--  (Excludes processing of messages sent before the program is started).
firstUpdateIDSession :: StateT Environment IO ()
firstUpdateIDSession = do
  obj <- getDataTg
  env <- get
  case obj of
    Nothing -> pure ()
    _ -> do
      lift $ getCurrentTime >>= print
      lift $ putStrLn "Connection established"
      let update_id' = case result <$> obj of
            Just [] -> 0
            Just messageDateLs -> (\(x : _) -> update_id x) (reverse messageDateLs)
            _ -> 0
      if lastUpdate env == 1
        then put $ Environment update_id' (userData env) (configuration env)
        else put $ Environment (1 + update_id') (userData env) (configuration env)

stringForCreateKeyboard :: MessageDate -> String -> String
stringForCreateKeyboard obj question =
  mconcat
    [ "/sendMessage?chat_id=",
      show $ chat_id $ chat $ message obj,
      "&text=",
      stringToUrl question,
      "&reply_markup=",
      stringToUrl $ LC.unpack $ encode createKeyboard
    ]

stringComment :: MessageDate -> String -> String
stringComment obj str =
  mconcat
    [ "/sendMessage?chat_id=",
      show $ chat_id $ chat $ message obj,
      "&text=",
      stringToUrl str
    ]

repeatMessageTg :: MessageDate -> StateT Environment IO (Response LC.ByteString)
repeatMessageTg obj = do
  env <- get
  let conf = configuration env
      messageId = show $ message_id $ message obj
      chatId = show $ chat_id $ chat $ message obj
      string =
        createStringRequest conf $
          mconcat
            [ "/copyMessage?chat_id=",
              chatId,
              "&from_chat_id=",
              chatId,
              "&message_id=",
              messageId
            ]
  writingLine DEBUG $ show string
  httpLBS string
