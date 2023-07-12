module Telegram.Handler where

import Control.Monad (replicateM_)
import qualified Data.ByteString.Lazy.Char8 as LC
import Lib (WorkHandle (..))
import Logger.Functions (writingLine)
import Network.HTTP.Simple (Response, httpLBS)
import Telegram.Functions (getDataTg, repeatMessageTg, stringComment, stringForCreateKeyboard)

-- Handle for work of Telegram's echobot.
handlerTg :: WorkHandle IO () (Response LC.ByteString)
handlerTg =
  WorkHandle
    { writingLineH = writingLine,
      getDataH = getDataTg,
      addNumberH = 1,
      stringForCreateKeyboardH = stringForCreateKeyboard,
      stringCommentH = stringComment,
      repeatMessageH = repeatMessageTg,
      sendHttpReqH = httpLBS,
      repeatActionH = replicateM_
    }
