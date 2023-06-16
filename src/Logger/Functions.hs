module Logger.Functions where

import Config
  ( Configuration (logOutput, priorityLevel),
  )
import Environment (getConfiguration)
import Logger.Data
  ( Priority (..),
    logFile,
    time,
  )

-- Function writes information to log.
writingLine :: Priority -> String -> IO ()
writingLine lvl str = do
  logLevel' <- priorityLevel <$> getConfiguration
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
