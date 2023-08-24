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
  obj <- getDataTg
  let conf = configuration env
  case obj of
    Nothing -> writingLine ERROR "Broken request!"
    _ -> do
      let arr = case result <$> obj of
            Just [messageDate] -> [messageDate]
            _ -> []
          update_id' = if null arr then 0 else (\(x : _) -> update_id x) (reverse arr)
      put $ Environment (1 + update_id') (userData env) conf
      _ <- get
      mapM_ (handleKeywords handlerTg) arr
      botLoop
