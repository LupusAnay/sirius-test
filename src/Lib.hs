-- |
-- Module : Lib
-- Description : Library for sirius-test project
module Lib
  ( app,
    Env (..),
    Config (..),
    writeSwaggerJSON,
  )
where

import App (Env (..), writeSwaggerJSON)
import Data (Config (..))
import Server (app)
