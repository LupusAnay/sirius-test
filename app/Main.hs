module Main where

import Hasql.Pool (acquire)
import Lib (Env (..), app)
import Network.Wai.Handler.Warp (run)
import Servant (JSON)
import Network.Wai.Middleware.Servant.Errors

main :: IO ()
main = do
  dbPool <-
    acquire
      ( 2,
        1,
        "postgresql://lupusanay:qwerty@localhost/sirius"
      )
  run 8001 $ errorMw @JSON @["error", "status"] $ app (Env dbPool)
