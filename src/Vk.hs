{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Vk where

import Control.Monad.State.Lazy
    ( MonadState(get, put), MonadTrans(lift), StateT )
import Data.Aeson
    ( eitherDecode, (.:), FromJSON(parseJSON), Value(Object) )
import qualified Data.Text as T 
import GHC.Generics (Generic)
import Network.HTTP.Simple
    ( parseRequest_, getResponseBody, getResponseStatusCode, Request )
import Lib
    ( Environment(..),
      Media,
      Priority(ERROR, DEBUG),
      WholeObject(..),
      MessageDate(..),
      Message(..),
      Chat(..),
      Configuration(groupIdVK, apiVKVersion),
      writingLine,
      configuration,
      myHost,
      myToken,
      handler,
      ifKeyWord,
      connection )


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
  { updates_type :: T.Text
  , updates_object :: ObjectVK
  }
  deriving Show

instance FromJSON Updates where
  parseJSON (Object v) = do
    updates_type <- v .: "type"
    updates_object <- v .: "object"
    pure $ Updates updates_type updates_object

newtype ObjectVK = ObjectVK
  {messageVK :: MessageVK}
  deriving Show
  
instance FromJSON ObjectVK where
  parseJSON (Object v) = do
    messageVK <- v .: "message"
    pure $ ObjectVK messageVK  

data MessageVK = MessageVK
  { from_id :: Int
  , messageVK_id :: Int
  , messageVK_text :: T.Text
  , messageVK_attachments :: [Media]
  }
  deriving Show
  
instance FromJSON MessageVK where
  parseJSON (Object v) = do
    from_id <- v .: "from_id"
    messageVK_id <- v .: "id"
    messageVK_text <- v .: "text"
    messageVK_attachments <- v .: "attachments" 
    pure $ MessageVK from_id messageVK_id messageVK_text messageVK_attachments  

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
  messageFromVk' <- messageFromVk obj
  pure $ MessageDate
    { update_id = num
    , Lib.message = messageFromVk' 
    }

messageFromVk :: Updates -> IO Message                                      
messageFromVk obj = do
  chatVk' <- chatVk obj
  pure $ Message
    { message_id = messageVK_id $ messageVK $ updates_object obj
    , chat = chatVk'
    , textM = Just $ messageVK_text $ messageVK $ updates_object obj
    , Lib.attachments = Just $ messageVK_attachments $ messageVK $ updates_object obj
    }

chatVk :: Updates -> IO Chat                                               
chatVk obj = do
  conf <- configuration
  pure $ Chat
    { chat_id = groupIdVK conf
    , username = T.pack $ show $ from_id $ messageVK $ updates_object obj
    }
 
getVkData :: T.Text -> T.Text -> T.Text -> IO (Maybe WholeObject)
getVkData s k t = do  -- Function for receiving data from
  x <- connection req 0                 --     the VK server.
  writingLine DEBUG $ show req
  let obj = eitherDecode $ getResponseBody x
  print $ getResponseBody x            -- Delete
  case obj of
    Left e -> do
      print $ getResponseBody x
      writingLine ERROR e
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
  x <- lift $ do
    getLongPollServerRequest' <- getLongPollServerRequest
    connection getLongPollServerRequest' 0
  let code = getResponseStatusCode x  
  if code == 200 
    then ( do
      let obj = eitherDecode $ getResponseBody x
      case obj of
        Left e -> lift $ writingLine ERROR $ show e
        Right v -> do
          let server' = server $ response v
              key' = key $ response v
              ts' = ts $ response v
          fun server' key' ts')
    else lift $ writingLine ERROR $ "statusCode " ++ show code          
 where
  fun s k t = do
    env <- get
    getVkDt <- lift $ getVkData s k t
    case getVkDt of
      Nothing -> botsLongPollAPI
      Just w -> do
        let arr = result w
            getVkData' lastUpdId = getVkData s k $ T.pack $ show lastUpdId
            update_id' = if null arr then 0 else (\(x:xs) -> update_id x) (reverse arr)
        put $ Environment update_id' (userData env)
        lift $ print arr                       -- Delete
        mapM_ (ifKeyWord handler getVkData') arr                      
        newEnv <- get
        fun s k $ T.pack $ show $ lastUpdate newEnv
