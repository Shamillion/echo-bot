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

data VkData = VkData
  { offset :: T.Text
  , updates :: [Updates]
  }
  deriving Show

instance FromJSON VkData where
  parseJSON (Object v) = do
    offset <- v .: "ts"
    updates  <- v .: "updates"    
    pure $ VkData offset updates
    
data Updates = Updates
  { type' :: T.Text
  , object :: ObjectVK
  } deriving Show
  
instance FromJSON Updates where
  parseJSON (Object v) = do
    type' <- v .: "type"
    object  <- v .: "object"    
    pure $ Updates type' object 

data ObjectVK = ObjectVK
  { message :: MessageVK } deriving (Show, Generic)
  
instance FromJSON ObjectVK       

data MessageVK = MessageVK
  { from_id :: Int
  , text :: T.Text
  } deriving (Show, Generic)
  
instance FromJSON MessageVK    

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

primaryData :: Either String VkResponse
primaryData = unsafePerformIO $ do
  x <- httpLBS getLongPollServer
  writingLine DEBUG $ show getLongPollServer
  let obj = eitherDecode $ getResponseBody x    
  case obj of
    Left _ -> do
      writingLine ERROR $ "User authorization failed!"    
      pure obj
    Right v -> do 
      writingLine DEBUG $ show v 
      pure obj
 

botsLongPollAPI :: IO ()
botsLongPollAPI = do
  let obj = primaryData
  case obj of
    Left _ -> pure ()      
    Right v -> do
      let key' = key $ response v
          server' = server $ response v
          ts' = ts $ response v
      x <- httpLBS $ parseRequest_ $ T.unpack $  
        mconcat [ server', "?act=a_check&key=", key', "&ts=", ts', "&wait=25" ] 
      print $ getResponseBody x














