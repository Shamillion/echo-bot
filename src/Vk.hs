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
--import Data.IORef
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
  , object' :: ObjectVK
  } deriving Show
  
instance FromJSON Updates where
  parseJSON (Object v) = do
    type' <- v .: "type"
    object'  <- v .: "object"    
    pure $ Updates type' object' 

data ObjectVK = ObjectVK
  { message :: MessageVK } deriving (Show, Generic)
  
instance FromJSON ObjectVK       

data MessageVK = MessageVK
  { from_id :: Int
  , id :: Int
  , text :: T.Text
  } deriving (Show, Generic)
  
instance FromJSON MessageVK    

wholeObjectVk :: VkData -> WholeObject
wholeObjectVk obj = WholeObject
  { ok = True
  , result = map (messageDateVk num) (updates obj)
  }
  where num = read $ T.unpack $ offset obj 
        
messageDateVk :: Int -> Updates -> MessageDate
messageDateVk num obj = MessageDate
  { update_id = num
  , Lib.message = messageVk obj
  }

messageVk :: Updates -> Message
messageVk obj = Message
  { message_id = Vk.id $ Vk.message $ object' obj  
  , chat = chatVk obj
  , textM = Just $ Vk.text $ Vk.message $ object' obj  
  }

chatVk :: Updates -> Chat
chatVk obj = Chat 
  { Lib.id = groupIdVK configuration
  , username = T.pack $ show $ from_id $ Vk.message $ object' obj  
  }

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
 
getVkData :: T.Text -> T.Text -> T.Text -> Maybe WholeObject
getVkData s k t = unsafePerformIO $ do
  x <- httpLBS $ parseRequest_  string
  writingLine DEBUG $ string      
  let obj = eitherDecode $ getResponseBody x    
  case obj of
    Left e -> do
      print $ getResponseBody x
      writingLine ERROR $ e --"Error Bots Long Poll API"    
      pure Nothing
    Right v -> do 
      writingLine DEBUG $ show v 
      case updates v of
        [] -> do
          print "Cycle" 
          pure $ getVkData s k $ offset v
        _ -> pure $ pure $ wholeObjectVk v 
  where string = T.unpack $  
                 mconcat [ s, "?act=a_check&key=", k, "&ts=", t, "&wait=25" ]  

botsLongPollAPI :: StateT Environment IO ()
botsLongPollAPI = do  
  let obj = primaryData
  case obj of
    Left _ -> pure ()      
    Right v -> do
      let server' = server $ response v
          key' = key $ response v
          ts' = ts $ response v
      fun server' key' ts' 
  where 
    fun s k t = do
      env <- get 
      case getVkData s k t of
        Nothing -> botsLongPollAPI 
        Just w  -> do
          let arr = result w
              getVkData' = getVkData s k $ T.pack $ show $ last arr
          put $ Environment (update_id $ last arr) (userData env)    
          lift $ print arr                          
          mapM_ (ifKeyWord getVkData') arr
          newEnv <- get
          fun s k $ T.pack $ show $ lastUpdate newEnv            












