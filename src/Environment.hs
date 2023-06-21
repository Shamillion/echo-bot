{-# LANGUAGE DerivingVia #-}

module Environment where

import Config
  ( Configuration (..),
    readConfigFile,
  )
import qualified Data.Map.Lazy as Map
import Data.String (IsString)

newtype UpdateID = UpdateID Int
  deriving (Eq)
  deriving (Num) via Int

instance Show UpdateID where
  show (UpdateID a) = show a

newtype Username = Username String
  deriving (Eq, Ord)
  deriving (IsString) via String

newtype NumRepeats = NumRepeats Int
  deriving (Eq, Show)

data Environment = Environment
  { lastUpdate :: UpdateID,
    userData :: Map.Map Username NumRepeats,
    configuration :: Configuration
  }

environment :: IO Environment
environment = do
  conf <- readConfigFile
  pure $
    Environment
      { lastUpdate = 0,
        userData = Map.singleton "" . NumRepeats . defaultRepeats $ conf,
        configuration = conf
      }
