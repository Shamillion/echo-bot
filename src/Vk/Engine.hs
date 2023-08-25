module Vk.Engine where

import Control.Monad.State.Lazy
  ( StateT,
    get,
    put,
  )
import Data
  ( DataFromServer (..),
    MessageDate (..),
  )
import Environment
  ( Environment (..),
    UpdateID (UpdateID),
  )
import Lib (handleKeywords)
import Logger.Data (Priority (ERROR))
import Logger.Functions (writingLine)
import Text.Read (readEither)
import Vk.Data
  ( VkKeyServerTs (..),
    VkResponse (response),
  )
import Vk.Functions
  ( getDataVk,
    getVkResponse,
  )
import Vk.Handler (handlerVk)

-- Main program cycle for VK.
botsLongPollAPI :: StateT Environment IO ()
botsLongPollAPI = do
  env <- get
  if lastUpdate env == 0
    then do
      resp <- getVkResponse
      updateID <-
        UpdateID <$> case readEither . ts . response $ resp of
          Right int -> pure int
          Left err -> writingLine ERROR err >> pure 0
      put $ Environment updateID (userData env) (configuration env)
      botsLongPollAPI
    else do
      maybeDataFromServer <- getDataVk
      case maybeDataFromServer of
        Nothing -> botsLongPollAPI
        Just dataFromServer -> do
          let messageDateLs = result dataFromServer
              update_id' = if null messageDateLs then 0 else (\(x : _) -> update_id x) (reverse messageDateLs)
          put $ Environment update_id' (userData env) (configuration env)
          mapM_ (handleKeywords handlerVk) messageDateLs
          botsLongPollAPI
