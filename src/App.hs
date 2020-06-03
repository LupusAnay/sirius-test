module App
  ( Env (..),
    AppM (..),
    MonadDB (..),
    writeSwaggerJSON,
  )
where

import Api
import Control.Lens ((&), (.~), (?~), (^.))
import Control.Monad.Except (ExceptT, MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger
import Control.Monad.Reader (MonadReader, ReaderT, ask)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Either.Combinators (mapLeft)
import Data.Generics.Labels ()
import Data.Swagger
import Database (MonadDB (..))
import Error (Error (..))
import GHC.Generics (Generic)
import Hasql.Pool (Pool)
import Hasql.Pool (use)
import Servant.Swagger
import Data (Config(..))
data Env
  = Env
      { pool :: Pool
      , config :: Config
      }
  deriving (Generic)


newtype AppM a
  = AppM
      { runAppM :: ReaderT Env (LoggingT (ExceptT Error IO)) a
      }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Env,
      MonadError Error,
      MonadLogger
    )

instance MonadDB AppM where
  runSession sess = do
    env <- ask
    let dbPool = env ^. #pool
    result <- liftIO $ use dbPool sess
    liftEither $ mapLeft DatabaseError result

writeSwaggerJSON :: IO ()
writeSwaggerJSON =
  BL8.writeFile "swagger.json" (encodePretty graphSwagger)

graphSwagger :: Swagger
graphSwagger =
  toSwagger api
    & info . title .~ "Graph API"
    & info . version .~ "1.0"
    & info . description ?~ "This is an API for sirius test"
    & info . license ?~ ("MIT" & url ?~ URL "http://mit.com")
