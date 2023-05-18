module Main where

import Control.Monad.State.Lazy (evalStateT, execStateT)
import Lib
  ( Configuration (messenger),
    Priority (INFO),
    configuration,
    endlessCycle,
    environment,
    firstUpdateIDSession,
    writingLine,
  )
import Vk (botsLongPollAPI)

main :: IO ()
main = do
  conf <- configuration
  env <- environment
  case messenger conf of
    "Error" -> putStrLn "Check out config.json"
    "TG" -> do
      mapM_ (\f -> f "Started Telegram echobot.") [putStrLn, writingLine INFO]
      newEnv <- execStateT firstUpdateIDSession env
      evalStateT endlessCycle newEnv
    _ -> do
      mapM_ (\f -> f "Started VK echobot.") [putStrLn, writingLine INFO]
      evalStateT botsLongPollAPI env
