module RequestBuilding where

import Config
  ( Configuration
      ( apiVKVersion,
        defaultRepeats,
        groupIdVK,
        messenger,
        messageRepeatCommand
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
import Data
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

getNumRepeats :: MessageDate -> Environment -> NumRepeats
getNumRepeats messageDate env =
  case Map.lookup usrName $ userData env of
    Nothing -> NumRepeats . defaultRepeats . configuration $ env
    Just num -> num
  where
    usrName = Username . username $ chat $ message messageDate

createQuestion :: MessageDate -> Environment -> String
createQuestion messageDate env = do
  let numRepeats = getNumRepeats messageDate env
  mconcat
    [ "Currently set to ",
      show numRepeats,
      " repetitions.\n",
      messageRepeatCommand . configuration $ env
    ]

convertStringToUrl :: String -> String
convertStringToUrl = Prelude.foldl encodingChar ""
  where
    encodingChar acc chr =
      acc ++ case chr of
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
        _ -> [chr]
