module Main where

import Control.Monad.State.Lazy
import Lib
import Vk
         

main :: IO ()
main = do 
  conf <- configuration 
  case messenger conf of
    "Error" -> print "Check out config.json"
    "TG"    -> do
      writingLine INFO "Started Telegram echobot."
      newEnv <- environment >>= execStateT firstUpdateIDSession  
      evalStateT endlessCycle newEnv
    _       -> do
      writingLine INFO "Started VK echobot."
      environment >>= evalStateT botsLongPollAPI 
      
