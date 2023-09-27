module Vk.Handler where

import Control.Monad (replicateM_)
import qualified Data.ByteString.Lazy.Char8 as LC
import Lib (WorkHandle (..))
import Logger.Functions (writingLine)
import Network.HTTP.Simple (Response, httpLBS)
import Vk.Functions
  ( getDataVk,
    repeatMessageVk,
    stringComment,
    stringForCreateKeyboard,
  )

handlerVk :: WorkHandle IO () (Response LC.ByteString)
handlerVk =
  WorkHandle
    { writingLineH = writingLine,
      getDataH = getDataVk,
      addNumberH = 0,
      stringForCreateKeyboardH = stringForCreateKeyboard,
      stringCommentH = stringComment,
      repeatMessageH = repeatMessageVk,
      sendHttpReqH = httpLBS,
      repeatActionH = replicateM_
    }
