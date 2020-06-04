-- |
-- Module : App
-- Description : Describes AppM monad and relevant bindings
module App
  ( Env (..),
    AppM (..),
    MonadDB (..),
    writeSwaggerJSON,
    graphSwagger,
  )
where

import Api
import Control.Lens ((&), (.~), (?~), (^.))
import Control.Monad.Except (ExceptT, MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Reader (MonadReader, ReaderT, ask)
import Data (Config (..))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Either.Combinators (mapBoth, mapLeft)
import Data.Generics.Labels ()
import Data.Swagger
  ( Swagger (..),
    URL (..),
    description,
    info,
    license,
    title,
    url,
    version,
  )
import Database (MonadDB (..))
import Error (Error (..))
import GHC.Generics (Generic)
import Hasql.Pool (Pool)
import Hasql.Pool (use)
import Servant.Swagger (toSwagger)

-- | Read-only app aata
data Env
  = Env
      { -- | Hasql pool
        pool :: Pool,
        -- | Application settings
        config :: Config
      }
  deriving (Generic)

-- | App monad
newtype AppM a
  = AppM
      { -- | Inverse of AppM
        runAppM :: ReaderT Env (LoggingT (ExceptT Error IO)) a
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
    result <- liftIO $ use (env ^. #pool) sess
    liftEither $ mapLeft DatabaseError result
  runSession_ sess = do
    env <- ask
    result <- liftIO $ use (env ^. #pool) sess
    liftEither $ mapBoth DatabaseError (\_ -> ()) result

-- | Write swagger to json file
writeSwaggerJSON :: IO ()
writeSwaggerJSON =
  BL8.writeFile "swagger.json" (encodePretty graphSwagger)

-- | Swagger definition generated from API
graphSwagger :: Swagger
graphSwagger =
  toSwagger api
    & info . title .~ "Graph API"
    & info . version .~ "1.0"
    & info . description ?~ "This is an API for sirius test"
    & info . license ?~ ("MIT" & url ?~ URL "http://mit.com")
