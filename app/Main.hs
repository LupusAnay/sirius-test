module Main where

import qualified Data.ByteString as BS
import Hasql.Pool (acquire)
import Lib (Env (..), app)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  dbPool <-
    acquire
      ( 2,
        1,
        BS.concat
          [ "host=",
            "localhost",
            " port=",
            "5432",
            " user=",
            "lupusanay",
            " dbname=",
            "sirius",
            " password=",
            "qwerty"
          ]
      )
  run 8001 $ app (Env dbPool)
