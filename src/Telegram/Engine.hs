module Telegram.Engine where

import Control.Monad.State.Lazy
  ( StateT,
    get,
    put,
  )
import Data
  ( MessageDate (update_id),
    DataFromServer (result),
  )
import Environment
  ( Environment
      ( Environment,
        configuration,
        userData
      ),
  )
import Lib (handleKeywords)
import Logger.Data (Priority (ERROR))
import Logger.Functions (writingLine)
import Telegram.Functions (getDataTg)
import Telegram.Handler (handlerTg)

-- Main program cycle for Telegram.
botLoop :: StateT Environment IO ()
botLoop = do
  env <- get
  maybeDataFromServer <- getDataTg
  let conf = configuration env
  case maybeDataFromServer of
    Nothing -> writingLine ERROR "Broken request!"
    _ -> do
      let messageDateLs = case result <$> maybeDataFromServer of
            Just [messageDate] -> [messageDate]
            _ -> []
          update_id' = if null messageDateLs then 0 else (\(x : _) -> update_id x) (reverse messageDateLs)
      put $ Environment (1 + update_id') (userData env) conf
      _ <- get
      mapM_ (handleKeywords handlerTg) messageDateLs
      botLoop
