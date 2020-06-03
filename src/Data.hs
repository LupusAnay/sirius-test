{-# LANGUAGE DeriveAnyClass #-}

module Data
  ( Id,
    Node (..),
    NewNode (..),
    Config (..),
  )
where

import Control.Monad.Logger (LogLevel (..))
import Data.Aeson ((.:), FromJSON (..))
import Data.Aeson (withObject)
import Data.Aeson.Types (ToJSON)
import Data.Swagger.Schema (ToSchema)
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import GHC.Generics (Generic)

type Id = Int

data Node
  = Node
      { id :: Id,
        label :: T.Text
      }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data NewNode
  = NewNode
      { label :: T.Text
      }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Config
  = Config
      { dbHost :: T.Text,
        dbPort :: Int,
        dbUser :: T.Text,
        dbName :: T.Text,
        dbPassword :: T.Text,
        dbPoolSize :: Int,
        dbPoolTimeout :: NominalDiffTime,
        serverPort :: Int,
        logLevel :: LogLevel
      }
  deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = withObject "config" $ \o ->
    Config
      <$> o .: "dbHost"
      <*> o .: "dbPort"
      <*> o .: "dbUser"
      <*> o .: "dbName"
      <*> o .: "dbPassword"
      <*> o .: "dbPoolSize"
      <*> o .: "dbPoolTimeout"
      <*> o .: "serverPort"
      <*> (logLevelFromText <$> o .: "logLevel")

logLevelFromText :: T.Text -> LogLevel
logLevelFromText "debug" = LevelDebug
logLevelFromText "info" = LevelInfo
logLevelFromText "warn" = LevelWarn
logLevelFromText "error" = LevelError
logLevelFromText level = LevelOther level
