module Main where

import Config
  ( Priority (INFO),
    currentMessenger,
    writingLine,
  )
import Control.Monad.State.Lazy
  ( evalStateT,
    execStateT,
  )
import Environment (environment)
import Lib
  ( endlessCycle,
    firstUpdateIDSession,
  )
import Vk (botsLongPollAPI)

main :: IO ()
main = do
  crntMsngr <- currentMessenger
  env <- environment
  case crntMsngr of
    "TG" -> do
      mapM_ (\f -> f "Started Telegram echobot.") [putStrLn, writingLine INFO]
      newEnv <- execStateT firstUpdateIDSession env
      evalStateT endlessCycle newEnv
    _ -> do
      mapM_ (\f -> f "Started VK echobot.") [putStrLn, writingLine INFO]
      evalStateT botsLongPollAPI env
