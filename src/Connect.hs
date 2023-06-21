module Connect where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad.State.Lazy
  ( StateT,
    lift,
    when,
  )
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Time (getCurrentTime)
import Environment (Environment)
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
connectToServer :: Request -> Int -> StateT Environment IO (Response LC.ByteString)
connectToServer req num = do
  x <- lift . try . httpLBS $ req
  writingLine DEBUG $ show req
  case x of
    Left cf@(HttpExceptionRequest _ (ConnectionFailure _)) ->
      errorProcessing cf num
    Left rt@(HttpExceptionRequest _ ResponseTimeout) ->
      errorProcessing rt num
    Left e -> do
      let err = show (e :: HttpException)
      writingLine ERROR err
      lift $ die err
    Right v -> do
      writingLine DEBUG $ show v
      lift $
        when (num /= 0) $ do
          getCurrentTime >>= print
          putStrLn "Connection restored"
      pure v
  where
    errorProcessing err n = do
      writingLine ERROR $ show (err :: HttpException)
      lift $ do
        when (n == 0) $ do
          getCurrentTime >>= print
          putStrLn "Connection Failure"
        putStrLn "Trying to set a connection... "
        threadDelay 1000000
      connectToServer req (n + 1)
