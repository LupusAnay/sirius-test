module Error (Error (..), ToServerError (..), logError) where

import Control.Exception (Exception)
import Control.Monad.Logger (LogLevel (..), MonadLogger, logErrorN, logInfoN)
import Data
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import GHC.Generics (Generic)
import Hasql.Pool (UsageError (..))
import Hasql.Session (CommandError (..), QueryError (..))
import qualified Hasql.Session as S (ResultError (..))
import Servant (ServerError (..), err400, err404, err500, err503)

data Error
  = DatabaseError UsageError
  | ObjectNotFoundError Id
  | LoopLinksForbidden
  deriving (Generic, Show)

instance Exception Error

class ToServerError e where
  convert :: LogLevel -> e -> ServerError

notFoundMessage :: Show a => a -> LBS.ByteString
notFoundMessage objId =
  LBS.concat ["Object with id ", LBS.pack $ show objId, " not found"]

instance ToServerError Error where
  convert level (DatabaseError e) = convert level e
  convert _ (ObjectNotFoundError objId) =
    err404 {errBody = notFoundMessage objId}
  convert _ (LoopLinksForbidden) =
    err400 {errBody = "Loop edges are not allowed"}

instance ToServerError UsageError where
  convert LevelDebug (ConnectionError e) =
    err503 {errBody = LBS.pack . show $ e}
  convert _ (ConnectionError _) = err503
  convert level (SessionError e) = convert level e

instance ToServerError QueryError where
  convert level (QueryError _ _ e) = convert level e

instance ToServerError CommandError where
  convert LevelDebug (ClientError (Just err)) =
    err500 {errBody = LBS.fromStrict err}
  convert _ (ClientError _) = err500
  convert LevelDebug (ClientError Nothing) =
    err500 {errBody = "Unknown database error: ClientError Nothing"}
  convert level (ResultError err) = convert level err

instance ToServerError S.ResultError where
  convert LevelDebug e = err500 {errBody = LBS.pack . show $ e}
  convert _ _ = err500

logError :: MonadLogger m => Error -> m ()
logError (DatabaseError e) = logErrorN (T.pack $ show e)
logError (ObjectNotFoundError objId) =
  logInfoN $ T.concat ["Requested object not found: ", T.pack . show $ objId]
logError (LoopLinksForbidden) =
  logInfoN "Loop links are forbidden"
