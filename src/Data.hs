{-# LANGUAGE DeriveAnyClass #-}

module Data
  ( Id,
    Node (..),
    NewNode (..),
    Config (..),
  )
where

import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.Swagger.Schema (ToSchema)
import qualified Data.Text as T
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
        dbPoolTimeout :: Int,
        serverPort :: Int,
        logLevel :: T.Text
      }
  deriving (Show, Generic, FromJSON, ToJSON)

