module Logger.Functions where

import Config
  ( Configuration
      ( logOutput,
        priorityLevel
      ),
  )
import Control.Monad.State.Lazy
  ( StateT,
    get,
    lift,
  )
import Environment
  ( Environment (configuration),
  )
import Logger.Data
  ( Priority (..),
    logFile,
    time,
  )

-- Function writes information to log.
writingLine :: Priority -> String -> StateT Environment IO ()
writingLine lvl str = do
  conf <- configuration <$> get
  if lvl >= priorityLevel conf
    then do
      time' <- lift time
      let string = time' ++ " UTC   " ++ showLevel lvl ++ " - " ++ str
          out = logOutput conf
      case out of
        "file" -> lift $ appendFile logFile $ string ++ "\n"
        _ -> lift $ print string
    else pure ()
  where
    showLevel priority = case priority of
      DEBUG -> "DEBUG  "
      INFO -> "INFO   "
      WARNING -> "WARNING"
      ERROR -> "ERROR  "
