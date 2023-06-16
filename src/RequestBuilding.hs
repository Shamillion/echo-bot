module RequestBuilding where

import Config
  ( Configuration
      ( apiVKVersion,
        defaultRepeats,
        groupIdVK,
        repeatMess, messenger
      ),
  )
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import Environment
  ( Environment (userData),
    NumRepeats (NumRepeats),
    Username (Username),
    getConfiguration,
    messengerHost,
    myHost,
    myToken,
  )
import Network.HTTP.Simple
  ( Request,
    parseRequest_,
  )
import Telegram.Data
  ( Chat (username),
    Message (chat),
    MessageDate (message),
  )

-- Request generation.
createStringRequest :: String -> IO Request
createStringRequest str = do
  conf <- getConfiguration
--  let crntMsngr = messenger conf
--  myHost' <- myHost
--  messengerHost' <- messengerHost
  myToken' <- myToken
  pure . parseRequest_ $
    case messenger conf of
      "TG" -> mconcat ["https://", messengerHost conf, myToken', str]
      _ ->
        mconcat
          [ "https://",
            myHost conf,
            "/method/messages.send?user_id=",
            str,
            "&peer_id=-",
            show $ groupIdVK conf,
            "&access_token=",
            myToken',
            "&v=",
            T.unpack $ apiVKVersion conf
          ]

getNumRepeats :: MessageDate -> Environment -> IO NumRepeats
getNumRepeats obj env = do
  conf <- getConfiguration
  pure $ case Map.lookup usrName $ userData env of
    Nothing -> NumRepeats $ defaultRepeats conf
    Just n -> n
  where
    usrName = Username . username $ chat $ message obj

createQuestion :: MessageDate -> Environment -> IO String
createQuestion obj env = do
  conf <- getConfiguration
  num <- getNumRepeats obj env
  pure $
    mconcat
      [ "Currently set to ",
        show num,
        " repetitions.\n",
        T.unpack $ repeatMess conf
      ]

stringToUrl :: String -> String
stringToUrl = Prelude.foldl encodingChar ""
  where
    encodingChar acc c =
      acc ++ case c of
        '{' -> "%7B"
        '}' -> "%7D"
        '[' -> "%5B"
        ']' -> "%5D"
        '\"' -> "%22"
        '\n' -> "%0A"
        ' ' -> "%20"
        ',' -> "%2C"
        '.' -> "%2E"
        ':' -> "%3A"
        _ -> [c]
