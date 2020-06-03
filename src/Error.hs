-- |
-- Module : Error
-- Description : SuperError type and relevant bindings
module Error
  ( Error (..),
    ToServerError (..),
    logError,
  )
where

import Control.Exception (Exception)
import Control.Monad.Logger (LogLevel (..), MonadLogger, logErrorN, logInfoN)
import Data
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text.Encoding
import GHC.Generics (Generic)
import Hasql.Pool (UsageError (..))
import Servant (ServerError (..), err400, err404, err500, err503)

-- | SuperError, wrapper for all error types in application
data Error
  = DatabaseError UsageError
  | ObjectNotFoundError Id
  | LoopLinksForbidden
  | LinkAlreadyExists Id Id
  deriving (Generic, Show)

instance Exception Error

-- | Converting SuperError to ServantError using LogLevel
class ToServerError e where
  convert :: LogLevel -> e -> ServerError

-- | Helper function to create message for not found error
notFoundMessage :: Show a => a -> LBS.ByteString
notFoundMessage objId =
  LBS.concat ["Object with id ", LBS.pack $ show objId, " not found"]

instance ToServerError Error where
  convert level (DatabaseError e) = convert level e
  convert _ (ObjectNotFoundError objId) =
    err404 {errBody = notFoundMessage objId}
  convert _ (LoopLinksForbidden) =
    err400 {errBody = "Loop edges are not allowed"}
  convert _ (LinkAlreadyExists id1 id2) =
    err400
      { errBody =
          LBS.pack $
            concat
              [ "Link connecting ",
                show id1,
                " and ",
                show id2,
                " already exist"
              ]
      }

instance ToServerError UsageError where
  convert LevelDebug e = err500 {errBody = LBS.pack . show $ e}
  convert _ (ConnectionError _) = err503
  convert _ _ = err500

-- | Write error in logger.
-- Used because different errors produce messages of different log level
logError :: MonadLogger m => Error -> m ()
logError (DatabaseError e) = logErrorN (T.pack $ show e)
logError (ObjectNotFoundError objId) =
  logInfoN . Text.Encoding.decodeUtf8 . LBS.toStrict $ notFoundMessage objId
logError (LoopLinksForbidden) = logInfoN "Loop links are forbidden"
logError (LinkAlreadyExists id1 id2) =
  logInfoN $ T.pack $
    concat
      [ "Link connecting ",
        show id1,
        " and ",
        show id2,
        " already exist"
      ]
