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
    "Error" -> print ("Check out config.json" :: String)
    "TG" -> do
      writingLine INFO "Started Telegram echobot."
      newEnv <- execStateT firstUpdateIDSession env
      evalStateT endlessCycle newEnv
    _ -> do
      writingLine INFO "Started VK echobot."
      evalStateT botsLongPollAPI env
