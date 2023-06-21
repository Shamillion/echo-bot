module RequestBuilding where

import Config
  ( Configuration
      ( apiVKVersion,
        defaultRepeats,
        groupIdVK,
        messenger,
        repeatMess
      ),
    messengerHost,
    myHost,
    myToken,
  )
import qualified Data.Map.Lazy as Map
import Environment
  ( Environment (configuration, userData),
    NumRepeats (NumRepeats),
    Username (Username),
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
createStringRequest :: Configuration -> String -> Request
createStringRequest conf str = parseRequest_ $
  case messenger conf of
    "TG" -> mconcat ["https://", messengerHost conf, myToken conf, str]
    _ ->
      mconcat
        [ "https://",
          myHost conf,
          "/method/messages.send?user_id=",
          str,
          "&peer_id=-",
          show $ groupIdVK conf,
          "&access_token=",
          myToken conf,
          "&v=",
          apiVKVersion conf
        ]

getNumRepeats :: MessageDate -> Environment -> IO NumRepeats
getNumRepeats obj env = do
  pure $ case Map.lookup usrName $ userData env of
    Nothing -> NumRepeats . defaultRepeats . configuration $ env
    Just n -> n
  where
    usrName = Username . username $ chat $ message obj

createQuestion :: MessageDate -> Environment -> IO String
createQuestion obj env = do
  num <- getNumRepeats obj env
  pure $
    mconcat
      [ "Currently set to ",
        show num,
        " repetitions.\n",
        repeatMess . configuration $ env
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
