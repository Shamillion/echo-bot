module Connect where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad.State.Lazy (when)
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Time (getCurrentTime)
import Logger.Data (Priority (DEBUG, ERROR))
import Logger.Functions (writingLine)
import Network.HTTP.Simple
  ( HttpException,
    Request,
    Response,
    httpLBS,
  )

-- Function for connecting to the server.
connectToServer :: Request -> Int -> IO (Response LC.ByteString)
connectToServer req num = do
  x <- try $ httpLBS req
  writingLine DEBUG $ show req
  case x of
    Left e -> do
      writingLine ERROR $ show (e :: HttpException)
      when (num == 0) $ do
        getCurrentTime >>= print
        putStrLn "Connection Failure"
        putStrLn "Trying to set a connection... "
      threadDelay 1000000
      connectToServer req (num + 1)
    Right v -> do
      writingLine DEBUG $ show v
      when (num /= 0) $ do
        getCurrentTime >>= print
        putStrLn "Connection restored"
      pure v
