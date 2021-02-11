{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple


myToken :: String
myToken = "xxx"

messengerHost :: String
messengerHost = "api.telegram.org/bot"

apiMethod :: String
apiMethod = "/getUpdates"

stringRequest :: Request
stringRequest = parseRequest_ $ 
  mconcat ["https://", messengerHost, myToken, apiMethod]

sendRequest :: IO LC.ByteString
sendRequest = do
    res <- httpLBS $ stringRequest 
    if (getResponseStatusCode res == 200 )
      then pure $ getResponseBody res
      else pure "Error! Broken request!"
    
    


main :: IO ()
main = do
    x <- sendRequest
    LC.putStrLn x


{-
{
  "ok":true,
  "result":[
    {
     "update_id":76765526,
     "message":{ "message_id":8,
                 "from":{ "id":195352543,
                          "is_bot":false,
                          "first_name":"\u0428\u0430\u043c\u0438\u043b\u044c",
                          "username":"S_a_m_a_i_l",
                          "language_code":"ru"},
                          "chat":{ "id":195352543,
                                   "first_name":"\u0428\u0430\u043c\u0438\u043b\u044c",
                                   "username":"S_a_m_a_i_l",
                                   "type":"private"
                                 },
                          "date":1612970774,
                          "text":"Hello my friend!"
               }
    },
    { "update_id":76765527,
      "message":{ "message_id":9,
                  "from":{ "id":195352543,
                           "is_bot":false,
                           "first_name":"\u0428\u0430\u043c\u0438\u043b\u044c",
                           "username":"S_a_m_a_i_l",
                           "language_code":"ru"
                         },
                  "chat":{ "id":195352543,
                           "first_name":"\u0428\u0430\u043c\u0438\u043b\u044c",
                           "username":"S_a_m_a_i_l",
                           "type":"private"
                         },
                  "date":1612972530,
                  "text":"Hi! Do you speak English?"
                }
    }]
}
-}
