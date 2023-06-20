module Main where

import Config (Configuration (messenger))
import Control.Monad.State.Lazy
  ( evalStateT,
    execStateT,
  )
import Environment
  ( Environment (configuration),
    environment,
  )
import Logger.Data (Priority (INFO))
import Logger.Functions (writingLine)
import Telegram.Engine (endlessCycle)
import Telegram.Functions (firstUpdateIDSession)
import Vk.Engine (botsLongPollAPI)

main :: IO ()
main = do
  env <- environment
  let conf = configuration env
  case messenger conf of
    "TG" -> do
      mapM_ (\f -> f "Started Telegram echobot.") [putStrLn, writingLine conf INFO]
      newEnv <- execStateT firstUpdateIDSession env
      evalStateT endlessCycle newEnv
    _ -> do
      mapM_ (\f -> f "Started VK echobot.") [putStrLn, writingLine conf INFO]
      evalStateT botsLongPollAPI env
