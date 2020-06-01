module Lib (someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Routes
  = Routes
      { _get :: route :- Capture "id" Int :> Get '[JSON] String
      }
