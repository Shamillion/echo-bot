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
      logString s = putStrLn s >> evalStateT (writingLine INFO s) env
  case messenger conf of
    "TG" -> do
      let str = "Started Telegram echobot."
      logString str
      newEnv <- execStateT firstUpdateIDSession env
      evalStateT endlessCycle newEnv
    _ -> do
      let str = "Started VK echobot."
      logString str
      evalStateT botsLongPollAPI env
