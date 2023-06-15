module Telegram.Engine where

import Control.Monad.State.Lazy
  ( MonadState (get, put),
    MonadTrans (lift),
    StateT,
    evalStateT,
  )
import Environment (Environment (Environment, userData))
import Lib (handler, ifKeyWord)
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
  obj <- lift $ evalStateT getData env
  let nothing _ = pure Nothing
  case obj of
    Nothing -> lift $ writingLine ERROR "Broken request!"
    _ -> do
      let arr = case result <$> obj of
            Just [x] -> [x]
            _ -> []
          update_id' = if null arr then 0 else (\(x : _) -> update_id x) (reverse arr)
      put $ Environment (1 + update_id') (userData env)
      _ <- get
      mapM_ (ifKeyWord handler nothing) arr
      endlessCycle
