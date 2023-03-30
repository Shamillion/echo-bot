{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Vk where

import Control.Monad.State.Lazy
import Data.Aeson
import qualified Data.Text as T 
import GHC.Generics (Generic)
import Network.HTTP.Simple
import Lib

data VkKeyServerTs = VkKeyServerTs       -- Data types for VK answer on    
  { key :: T.Text                        --   getLongPollServer request.
  , server :: T.Text
  , ts :: T.Text
  } deriving (Show, Generic, FromJSON)

newtype VkResponse = VkResponse
  { response :: VkKeyServerTs } 
  deriving (Show, Generic, FromJSON)   

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
  deriving Show

instance FromJSON Updates where
  parseJSON (Object v) = do
    type' <- v .: "type"
    object' <- v .: "object"
    pure $ Updates type' object'

newtype ObjectVK = ObjectVK
  {message :: MessageVK}
  deriving (Show, Generic, FromJSON)

data MessageVK = MessageVK
  { from_id :: Int
  , id :: Int
  , text :: T.Text
  , attachments :: [Media]
  }
  deriving (Show, Generic, FromJSON)

wholeObjectVk :: VkData -> IO WholeObject -- Functions for converting VK's   
wholeObjectVk obj = do                   --   data to Telegrams's data.
  ls <- mapM (messageDateVk num) $ updates obj
  pure $ WholeObject 
    { ok = True
    , result = ls
    }
 where
  num = read $ T.unpack $ offset obj

messageDateVk :: Int -> Updates -> IO MessageDate                            
messageDateVk num obj = do 
  messageVk' <- messageVk obj
  pure $ MessageDate
    { update_id = num
    , Lib.message = messageVk' 
    }

messageVk :: Updates -> IO Message                                      
messageVk obj = do
  chatVk' <- chatVk obj
  pure $ Message
    { message_id = Vk.id $ Vk.message $ object' obj
    , chat = chatVk'
    , textM = Just $ Vk.text $ Vk.message $ object' obj
    , Lib.attachments = Just $ Vk.attachments $ Vk.message $ object' obj
    }

chatVk :: Updates -> IO Chat                                               
chatVk obj = do
  conf <- configuration
  pure $ Chat
    { Lib.id = groupIdVK conf
    , username = T.pack $ show $ from_id $ Vk.message $ object' obj
    }
 
getVkData :: T.Text -> T.Text -> T.Text -> IO (Maybe WholeObject)
getVkData s k t = do  -- Function for receiving data from
  x <- connection req 0                 --     the Telegram server.
  writingLine DEBUG $ show req
  let obj = eitherDecode $ getResponseBody x
  print $ getResponseBody x            -- Delete
  case obj of
    Left e -> do
      print $ getResponseBody x
      writingLine ERROR $ e
      pure Nothing
    Right v -> do
      writingLine DEBUG $ show v
      case updates v of
        [] -> getVkData s k $ offset v
        _ -> pure <$> wholeObjectVk v
 where
  req =
    parseRequest_ $
      T.unpack $
        mconcat [s, "?act=a_check&key=", k, "&ts=", t, "&wait=25"]
        
getLongPollServerRequest :: IO Request                                    -------------
getLongPollServerRequest = do
  conf <- configuration
  myHost' <- myHost
  myToken' <- myToken
  pure . parseRequest_ $
    mconcat
      [ "https://"
      , myHost'
      , "/method/groups.getLongPollServer?group_id="
      , show $ groupIdVK conf
      , "&access_token="
      , myToken'
      , "&v="
      , T.unpack $ apiVKVersion conf
      ]

botsLongPollAPI :: StateT Environment IO () -- Main program cycle for VK.
botsLongPollAPI = do
  envir <- get
  x <- lift $ do
    getLongPollServerRequest' <- getLongPollServerRequest
    connection getLongPollServerRequest' 0
  let code = getResponseStatusCode x
  writing <- lift $ writingLine ERROR $ "statusCode" ++ show code
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
    getVkDt <- lift $ getVkData s k t
    case getVkDt of
      Nothing -> botsLongPollAPI
      Just w -> do
        let arr = result w
            getVkData' lastUpdId = getVkData s k $ T.pack $ show lastUpdId
        put $ Environment (update_id $ last arr) (userData env)
        lift $ print arr                       -- Delete
        mapM_ (ifKeyWord handler getVkData') arr                      ----------------
        newEnv <- get
        fun s k $ T.pack $ show $ lastUpdate newEnv
