{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Vk where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State.Lazy
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T hiding (last)
import Data.Time
import GHC.Generics (Generic)
import Network.HTTP.Simple
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Lib

data VkKeyServerTs = VkKeyServerTs
  { key :: T.Text
  , server :: T.Text
  , ts :: T.Text
  } deriving (Show, Generic)
  
instance FromJSON VkKeyServerTs 

data VkResponse = VkResponse
  { response :: VkKeyServerTs } 
  deriving (Show, Generic)  

instance FromJSON VkResponse   

--data VkData = VkData
  --{ ts :: T.Text
  --, updates :: Message
  --}
  --deriving (Show, Generic)

getLongPollServer :: Request
getLongPollServer = 
  parseRequest_ $  
    mconcat ["https://"
            , myHost
            , "/method/groups.getLongPollServer?group_id="
            , show $ groupIdVK configuration
            , "&access_token="            
            , myToken
            , "&v="
            , T.unpack $ apiVKVersion configuration
            ] 
















