module Connect where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad.State.Lazy (when)
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Time (getCurrentTime)
import Logger.Data (Priority (DEBUG, ERROR))
import Logger.Functions (writingLine)
import Network.HTTP.Conduit
  ( HttpExceptionContent (ConnectionFailure),
  )
import Network.HTTP.Simple
  ( HttpException (HttpExceptionRequest),
    Request,
    Response,
    httpLBS,
  )
import System.Exit (die)

-- Function for connecting to the server.
connectToServer :: Request -> Int -> IO (Response LC.ByteString)
connectToServer req num = do
  x <- try $ httpLBS req
  writingLine DEBUG $ show req
  case x of
    Left cf@(HttpExceptionRequest _ (ConnectionFailure _)) -> do
      writingLine ERROR $ show (cf :: HttpException)
      when (num == 0) $ do
        getCurrentTime >>= print
        putStrLn "Connection Failure"
      putStrLn "Trying to set a connection... "
      threadDelay 1000000
      connectToServer req (num + 1)
    Left e -> do
      let err = show (e :: HttpException)
      writingLine ERROR err
      die err
    Right v -> do
      writingLine DEBUG $ show v
      when (num /= 0) $ do
        getCurrentTime >>= print
        putStrLn "Connection restored"
      pure v
