{-# LANGUAGE DeriveAnyClass #-}

module Data
  ( Id,
    Node (..),
    NewNode (..),
  )
where

import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)

type Id = Int

data Node
  = Node
      { id :: Id,
        label :: T.Text
      }
  deriving (Generic, ToJSON, FromJSON)

data NewNode
  = NewNode
      {label :: T.Text}
  deriving (Generic, ToJSON, FromJSON)
