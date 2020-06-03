module Error (Error (..), ToServerError (..)) where

import Control.Exception (Exception)
import Data
import qualified Data.ByteString.Lazy.Char8 as LBS
import GHC.Generics (Generic)
import Hasql.Pool (UsageError (..))
import Hasql.Session (CommandError (..), QueryError (..))
import qualified Hasql.Session as S (ResultError (..))
import Servant (ServerError (..), err400, err404, err500, err503)

data Error = DatabaseError UsageError | ObjectNotFoundError Id
  deriving (Generic, Show)

instance Exception Error

class ToServerError e where
  convert :: e -> ServerError

-- TODO: Rework to support debug/production setting

notFoundMessage :: Show a => a -> LBS.ByteString
notFoundMessage objId =
  LBS.concat ["Object with id ", LBS.pack $ show objId, " not found"]

instance ToServerError Error where
  convert (DatabaseError e) = convert e
  convert (ObjectNotFoundError objId) = err404 {errBody = notFoundMessage objId}

instance ToServerError UsageError where
  convert (ConnectionError e) = err503 {errBody = LBS.pack . show $ e}
  convert (SessionError e) = convert e

instance ToServerError QueryError where
  convert (QueryError _ _ e) = convert e

instance ToServerError CommandError where
  convert (ClientError (Just err)) = err500 {errBody = LBS.fromStrict err}
  convert (ClientError Nothing) = err500 {errBody = "Unknown database error"}
  convert (ResultError err) = convert err

instance ToServerError S.ResultError where
  convert (S.ServerError _ message _ _) =
    err400 {errBody = LBS.fromStrict message}
  convert (S.UnexpectedResult _) =
    err500 {errBody = "Unexpected Result"}
  convert (S.RowError _ err) =
    err500 {errBody = LBS.pack . show $ err}
  convert (S.UnexpectedAmountOfRows _) =
    err500 {errBody = "Unexpected amount of rows"}
