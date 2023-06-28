module Vk.Handler where

import qualified Data.ByteString.Lazy.Char8 as LC
import Lib (WorkHandle (getDataH))
import Network.HTTP.Simple (Response)
import Telegram.Handler (handlerTg)
import Vk.Functions (getDataVk)

-- Handle for work of Vk echobot.
handlerVk :: WorkHandle IO () (Response LC.ByteString)
handlerVk =
  handlerTg
    { getDataH = getDataVk
    }
