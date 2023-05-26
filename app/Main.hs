module Main where

import Control.Monad.State.Lazy (evalStateT, execStateT)
import Lib
  ( Priority (INFO),
    currentMessenger,
    endlessCycle,
    environment,
    firstUpdateIDSession,
    writingLine,
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
