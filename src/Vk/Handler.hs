module Vk.Handler where

import qualified Data.ByteString.Lazy.Char8 as LC
import Lib
  ( WorkHandle (..),
    sendComment,
    sendKeyboard,
    sendRepeats,
    wordIsRepeat,
  )
import Logger.Functions (writingLine)
import Network.HTTP.Simple (Response)
import Vk.Functions
  ( getDataVk,
    repeatMessageVk,
    stringComment,
    stringForCreateKeyboard,
  )

-- Handle for work of Vk echobot.
handlerVk :: WorkHandle IO () (Response LC.ByteString)
handlerVk =
  WorkHandle
    { writingLineH = writingLine,
      sendKeyboardH = sendKeyboard,
      sendCommentH = sendComment,
      sendRepeatsH = sendRepeats,
      wordIsRepeatH = wordIsRepeat,
      getDataH = getDataVk,
      addNumberH = 0,
      stringForCreateKeyboardH = stringForCreateKeyboard,
      stringCommentH = stringComment,
      repeatMessageH = repeatMessageVk
    }
