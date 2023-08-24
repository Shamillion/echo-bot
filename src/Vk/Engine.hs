module Vk.Engine where

import Control.Monad.State.Lazy
  ( StateT,
    get,
    put,
  )
import Data
  ( MessageDate (..),
    DataFromServer (..),
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
      num <-
        UpdateID <$> case readEither . ts . response $ resp of
          Right int -> pure int
          Left err -> writingLine ERROR err >> pure 0
      put $ Environment num (userData env) (configuration env)
      botsLongPollAPI
    else do
      getDtVk <- getDataVk
      case getDtVk of
        Nothing -> botsLongPollAPI
        Just wholeObj -> do
          let arr = result wholeObj
              update_id' = if null arr then 0 else (\(x : _) -> update_id x) (reverse arr)
          put $ Environment update_id' (userData env) (configuration env)
          mapM_ (handleKeywords handlerVk) arr
          botsLongPollAPI
