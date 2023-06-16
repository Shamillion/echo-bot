module Main where

import Control.Monad.State.Lazy
  ( evalStateT,
    execStateT,
  )
import Environment
  ( currentMessenger,
    environment,
  )
import Logger.Data (Priority (INFO))
import Logger.Functions (writingLine)
import Telegram.Engine (endlessCycle)
import Telegram.Functions (firstUpdateIDSession)
import Vk.Engine (botsLongPollAPI)

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
