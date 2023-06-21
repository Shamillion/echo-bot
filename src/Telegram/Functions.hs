module Telegram.Functions where

import Config
  ( messengerHost,
    myToken,
  )
import Connect (connectToServer)
import Control.Monad.State.Lazy
  ( MonadState (get, put),
    MonadTrans (lift),
    StateT,
  )
import Data.Aeson (decode)
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
import Logger.Data (Priority (ERROR))
import Logger.Functions (writingLine)
import Network.HTTP.Simple
  ( getResponseBody,
    getResponseStatusCode,
    parseRequest_,
  )
import Telegram.Data
  ( MessageDate (update_id),
    WholeObject (result),
  )

-- Update request string for Telegram.
createStringGetUpdates :: UpdateID -> String
createStringGetUpdates (UpdateID num) =
  mconcat ["/getUpdates?offset=", show num, "&timeout=1"]

-- Function for getting data from Telegram's server.
getData :: StateT Environment IO (Maybe WholeObject)
getData = do
  env <- get
  let str = createStringGetUpdates . lastUpdate $ env
      conf = configuration env
      req =
        parseRequest_ $
          mconcat ["https://", messengerHost conf, myToken conf, str]
  x <- lift $ connectToServer conf req 0
  let code = getResponseStatusCode x
  if code == 200
    then
      ( do
          let obj = decode $ getResponseBody x
          case result <$> obj of
            Just [] -> do
              put $ Environment 1 (userData env) (configuration env)
              getData
            _ -> do
              pure obj
      )
    else lift $ do
      writingLine conf ERROR $ "statusCode " ++ show code
      pure Nothing

-- Function for getting update_id  for the first time.
--  (Excludes processing of messages sent before the program is started).
firstUpdateIDSession :: StateT Environment IO ()
firstUpdateIDSession = do
  obj <- getData
  env <- get
  case obj of
    Nothing -> pure ()
    _ -> do
      lift $ getCurrentTime >>= print
      lift $ putStrLn "Connection established"
      let update_id' = case result <$> obj of
            Just [] -> 0
            Just md -> (\(x : _) -> update_id x) (reverse md)
            _ -> 0
      if lastUpdate env == 1
        then put $ Environment update_id' (userData env) (configuration env)
        else put $ Environment (1 + update_id') (userData env) (configuration env)
