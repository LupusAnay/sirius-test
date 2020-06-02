module App
  ( Env (..),
    AppM (..),
    MonadDB (..),
  )
where

import Control.Lens ((^.))
import Control.Monad.Except (ExceptT, MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, ask)
import Data.Either.Combinators (mapLeft)
import Data.Generics.Labels ()
import Database (MonadDB (..))
import Error (Error (..))
import GHC.Generics (Generic)
import Hasql.Pool (Pool)
import Hasql.Pool (use)

data Env
  = Env
      { pool :: Pool
      }
  deriving (Generic)

newtype AppM a
  = AppM
      { runAppM :: ReaderT Env (ExceptT Error IO) a
      }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Env,
      MonadError Error
    )

instance MonadDB AppM where
  runSession sess = do
    env <- ask
    let dbPool = env ^. #pool
    result <- liftIO $ use dbPool sess
    liftEither $ mapLeft DatabaseError result
