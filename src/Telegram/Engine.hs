module Telegram.Engine where

import Control.Monad.State.Lazy
  ( StateT,
    get,
    put,
  )
import Environment
  ( Environment
      ( Environment,
        configuration,
        userData
      ),
  )
import Lib
  ( handler,
    ifKeyWord,
  )
import Logger.Data (Priority (ERROR))
import Logger.Functions (writingLine)
import Telegram.Data
  ( MessageDate (update_id),
    WholeObject (result),
  )
import Telegram.Functions (getData)

-- Main program cycle for Telegram.
endlessCycle :: StateT Environment IO ()
endlessCycle = do
  env <- get
  obj <- getData
  let conf = configuration env
      nothing _ = pure Nothing
  case obj of
    Nothing -> writingLine ERROR "Broken request!"
    _ -> do
      let arr = case result <$> obj of
            Just [messageDate] -> [messageDate]
            _ -> []
          update_id' = if null arr then 0 else (\(x : _) -> update_id x) (reverse arr)
      put $ Environment (1 + update_id') (userData env) conf
      _ <- get
      mapM_ (ifKeyWord handler nothing) arr
      endlessCycle
