module Connect where

import Config (Configuration)
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad.State.Lazy (when)
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Time (getCurrentTime)
import Logger.Data (Priority (DEBUG, ERROR))
import Logger.Functions (writingLine)
import Network.HTTP.Conduit
  ( HttpExceptionContent (ConnectionFailure, ResponseTimeout),
  )
import Network.HTTP.Simple
  ( HttpException (HttpExceptionRequest),
    Request,
    Response,
    httpLBS,
  )
import System.Exit (die)

-- Function for connecting to the server.
connectToServer :: Configuration -> Request -> Int -> IO (Response LC.ByteString)
connectToServer conf req num = do
  x <- try $ httpLBS req
  writingLine conf DEBUG $ show req
  case x of
    Left cf@(HttpExceptionRequest _ (ConnectionFailure _)) ->
      errorProcessing cf num
    Left rt@(HttpExceptionRequest _ ResponseTimeout) ->
      errorProcessing rt num
    Left e -> do
      let err = show (e :: HttpException)
      writingLine conf ERROR err
      die err
    Right v -> do
      writingLine conf DEBUG $ show v
      when (num /= 0) $ do
        getCurrentTime >>= print
        putStrLn "Connection restored"
      pure v
  where
    errorProcessing err n = do
      writingLine conf ERROR $ show (err :: HttpException)
      when (n == 0) $ do
        getCurrentTime >>= print
        putStrLn "Connection Failure"
      putStrLn "Trying to set a connection... "
      threadDelay 1000000
      connectToServer conf req (n + 1)
