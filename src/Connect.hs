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
  ( HttpExceptionContent
      ( ConnectionFailure,
        ResponseTimeout
      ),
  )
import Network.HTTP.Simple
  ( HttpException (HttpExceptionRequest),
    Request,
    Response,
    httpLBS,
  )
import System.Exit (die)

connectToServer :: Request -> Int -> StateT Environment IO (Response LC.ByteString)
connectToServer req retries = do
  resp <- lift . try . httpLBS $ req
  writingLine DEBUG $ show req
  case resp of
    Left cf@(HttpExceptionRequest _ (ConnectionFailure _)) ->
      errorProcessing cf retries
    Left rt@(HttpExceptionRequest _ ResponseTimeout) ->
      errorProcessing rt retries
    Left otherError -> do
      let err = show (otherError :: HttpException)
      writingLine ERROR err
      lift $ die err
    Right ans -> do
      writingLine DEBUG $ show ans
      lift $
        when (retries /= 0) $ do
          getCurrentTime >>= print
          putStrLn "Connection restored"
      pure ans
  where
    errorProcessing err iter = do
      writingLine ERROR $ show (err :: HttpException)
      lift $ do
        when (iter == 0) $ do
          getCurrentTime >>= print
          putStrLn "Connection Failure"
        putStrLn "Trying to set a connection... "
        threadDelay 1000000
      connectToServer req (iter + 1)
