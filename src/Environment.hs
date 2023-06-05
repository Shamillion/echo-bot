{-# LANGUAGE DerivingVia #-}

module Environment where

import Config
  ( Configuration (defaultRepeats),
    configuration,
  )
import qualified Data.Map.Lazy as Map
import Data.String (IsString)
import qualified Data.Text as T

newtype UpdateID = UpdateID Int
  deriving (Eq)
  deriving (Num) via Int

instance Show UpdateID where
  show (UpdateID a) = show a

newtype Username = Username T.Text
  deriving (Eq, Ord)
  deriving (IsString) via T.Text

newtype NumRepeats = NumRepeats Int
  deriving (Eq, Show)

data Environment = Environment
  { lastUpdate :: UpdateID,
    userData :: Map.Map Username NumRepeats
  }

environment :: IO Environment
environment =
  (Environment 0 . Map.singleton "" . NumRepeats)
    . defaultRepeats
    <$> configuration
