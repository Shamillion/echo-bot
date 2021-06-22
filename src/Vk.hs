{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Vk where

import Control.Monad.State.Lazy
import Data.Aeson
import qualified Data.Text as T 
import GHC.Generics (Generic)
import Network.HTTP.Simple
import System.IO.Unsafe (unsafePerformIO)
import Lib

data VkKeyServerTs = VkKeyServerTs       -- Data types for VK answer on    
  { key :: T.Text                        --   getLongPollServer request.
  , server :: T.Text
  , ts :: T.Text
  } deriving (Show, Generic)
  
instance FromJSON VkKeyServerTs 

newtype VkResponse = VkResponse
  { response :: VkKeyServerTs } 
  deriving (Show, Generic)  

instance FromJSON VkResponse   

data VkData = VkData                    -- Data types for VK answer on
  { offset :: T.Text                    --   BotsLongPollAPI request.
  , updates :: [Updates]
  }
  deriving Show

instance FromJSON VkData where
  parseJSON (Object v) = do
    offset <- v .: "ts"
    updates <- v .: "updates"
    pure $ VkData offset updates

data Updates = Updates
  { type' :: T.Text
  , object' :: ObjectVK
  }
  deriving (Show)

instance FromJSON Updates where
  parseJSON (Object v) = do
    type' <- v .: "type"
    object' <- v .: "object"
    pure $ Updates type' object'

newtype ObjectVK = ObjectVK
  {message :: MessageVK}
  deriving (Show, Generic)

instance FromJSON ObjectVK

data MessageVK = MessageVK
  { from_id :: Int
  , id :: Int
  , text :: T.Text
  }
  deriving (Show, Generic)

instance FromJSON MessageVK

wholeObjectVk :: VkData -> WholeObject -- Functions for converting VK's
wholeObjectVk obj =                    --   data to Telegrams's data.
  WholeObject 
    { ok = True
    , result = map (messageDateVk num) (updates obj)
    }
 where
  num = read $ T.unpack $ offset obj

messageDateVk :: Int -> Updates -> MessageDate
messageDateVk num obj =
  MessageDate
    { update_id = num
    , Lib.message = messageVk obj
    }

messageVk :: Updates -> Message
messageVk obj =
  Message
    { message_id = Vk.id $ Vk.message $ object' obj
    , chat = chatVk obj
    , textM = Just $ Vk.text $ Vk.message $ object' obj
    }

chatVk :: Updates -> Chat
chatVk obj =
  Chat
    { Lib.id = groupIdVK configuration
    , username = T.pack $ show $ from_id $ Vk.message $ object' obj
    }
 
getVkData :: T.Text -> T.Text -> T.Text -> Maybe WholeObject
getVkData s k t = unsafePerformIO $ do  -- Function for receiving data from
  x <- connection req 0                 --     the Telegram server.
  writingLine DEBUG $ show req
  let obj = eitherDecode $ getResponseBody x
  case obj of
    Left e -> do
      print $ getResponseBody x
      writingLine ERROR $ e
      pure Nothing
    Right v -> do
      writingLine DEBUG $ show v
      case updates v of
        [] -> pure $ getVkData s k $ offset v
        _ -> pure $ pure $ wholeObjectVk v
 where
  req =
    parseRequest_ $
      T.unpack $
        mconcat [s, "?act=a_check&key=", k, "&ts=", t, "&wait=25"]
getLongPollServerRequest :: Request
getLongPollServerRequest =
  parseRequest_ $
    mconcat
      [ "https://"
      , myHost
      , "/method/groups.getLongPollServer?group_id="
      , show $ groupIdVK configuration
      , "&access_token="
      , myToken
      , "&v="
      , T.unpack $ apiVKVersion configuration
      ]

botsLongPollAPI :: StateT Environment IO () -- Main program cycle for VK.
botsLongPollAPI = do
  envir <- get
  let x = unsafePerformIO $ connection getLongPollServerRequest 0
      code = getResponseStatusCode x
      writing = unsafePerformIO $ writingLine ERROR $ "statusCode" ++ show code
  case code == 200 of
    False -> lift $ writingLine ERROR $ "statusCode " ++ show code
    _ -> do
      let obj = eitherDecode $ getResponseBody x
      case obj of
        Left e -> lift $ writingLine ERROR $ show e
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
      Just w -> do
        let arr = result w
            getVkData' lastUpdId = getVkData s k $ T.pack $ show lastUpdId
        put $ Environment (update_id $ last arr) (userData env)
        lift $ print arr -- Delete
        mapM_ (ifKeyWord handler getVkData') arr
        newEnv <- get
        fun s k $ T.pack $ show $ lastUpdate newEnv
