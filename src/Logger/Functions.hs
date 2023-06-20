module Logger.Functions where

import Config
  ( Configuration
      ( logOutput,
        priorityLevel
      ),
  )
import Logger.Data
  ( Priority (..),
    logFile,
    time,
  )

-- Function writes information to log.
writingLine :: Configuration -> Priority -> String -> IO ()
writingLine conf lvl str = do
  if lvl >= priorityLevel conf
    then do
      t <- time
      let string = t ++ " UTC   " ++ showLevel lvl ++ " - " ++ str
          out = logOutput conf
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
