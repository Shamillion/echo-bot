module Logger.Functions where

import Config
  ( Configuration (logOutput, priorityLevel),
    getConfiguration,
  )
import Logger.Data
  ( Priority (..),
    logFile,
    time,
  )

-- Logging level.
logLevel :: IO Priority
logLevel = priorityLevel <$> getConfiguration

-- Function writes information to log.
writingLine :: Priority -> String -> IO ()
writingLine lvl str = do
  logLevel' <- logLevel
  if lvl >= logLevel'
    then do
      t <- time
      let string = t ++ " UTC   " ++ showLevel lvl ++ " - " ++ str
      out <- logOutput <$> getConfiguration
      case out of
        "file" -> appendFile logFile $ string ++ "\n"
        _ -> print string
    else pure ()
  where
    showLevel val = case val of
      DEBUG -> "DEBUG  "
      INFO -> "INFO   "
      WARNING -> "WARNING"
      ERROR -> "ERROR  "
