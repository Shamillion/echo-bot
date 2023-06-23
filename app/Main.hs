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
      logString str = putStrLn str >> evalStateT (writingLine INFO str) env
  case messenger conf of
    "TG" -> do
      logString "Started Telegram echobot."
      newEnv <- execStateT firstUpdateIDSession env
      evalStateT endlessCycle newEnv
    _ -> do
      logString "Started VK echobot."
      evalStateT botsLongPollAPI env
