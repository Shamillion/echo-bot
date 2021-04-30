{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Network.HTTP.Simple

import Control.Monad.State.Lazy
import Lib
import Vk
                

main :: IO ()
main = do  
  case messenger configuration of
    "Error" -> print "Check out config.json"
    "TG"    -> do
      writingLine INFO "Started Telegram echobot."
      newEnv <- execStateT firstUpdateIDSession environment 
      evalStateT endlessCycle newEnv
    _       -> do
      writingLine INFO "Started VK echobot."
      botsLongPollAPI
      
      
  
  



