module Telegram.Handler where

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
import Telegram.Functions (getDataTg)

-- Handle for work of Telegram's echobot.
handlerTg :: WorkHandle IO () (Response LC.ByteString)
handlerTg =
  WorkHandle
    { writingLineH = writingLine,
      sendKeyboardH = sendKeyboard,
      sendCommentH = sendComment,
      sendRepeatsH = sendRepeats,
      wordIsRepeatH = wordIsRepeat,
      getDataH = getDataTg
    }
