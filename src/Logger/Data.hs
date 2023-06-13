{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Logger.Data where

import Data.Aeson (FromJSON)
import Data.Time (getCurrentTime)
import GHC.Generics (Generic)

-- Data type for the logger.
data Priority = DEBUG | INFO | WARNING | ERROR
  deriving (Show, Eq, Ord, Generic, FromJSON)

-- Getting current time for the logger.
time :: IO String
time = take 19 . show <$> getCurrentTime

-- Name of the logfile.
logFile :: String
logFile = "log.log"
