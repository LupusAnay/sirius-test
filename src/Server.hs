module Server
  ( app,
  )
where

import Api
  ( GraphRoutes (..),
    LinkRoutes (..),
    NodeRoutes (..),
    Routes (..),
  )
import App
import Control.Monad.Except (MonadError (throwError), catchError, withExceptT)
import Control.Monad.Logger
  ( MonadLogger,
    logErrorN,
    logInfoN,
    runStderrLoggingT,
  )
import Control.Monad.Reader (runReaderT)
import Data.Generics.Labels ()
import qualified Data.Text as T
import Error
import qualified Handlers as H
import Servant (Application, Handler (..))
import Servant.API.Generic (toServant)
import Servant.Server.Generic (AsServerT, genericServeT)

nodesServer :: NodeRoutes (AsServerT AppM)
nodesServer =
  NodeRoutes
    { listNodes = H.listNodes,
      createNode = H.createNode,
      deleteNode = H.deleteNode,
      changeLabel = H.changeNodeLabel,
      listNeighbours = H.listNeighbours
    }

linksServer :: LinkRoutes (AsServerT AppM)
linksServer =
  LinkRoutes
    { createLink = H.createLink
    }

graphServer :: GraphRoutes (AsServerT AppM)
graphServer =
  GraphRoutes
    { nodes = toServant nodesServer,
      links = toServant linksServer
    }

routesServer :: Routes (AsServerT AppM)
routesServer =
  Routes
    { graph = toServant graphServer
    }

nt :: Env -> AppM a -> Handler a
nt env appValue = Handler $ handler
  where
    reader = runAppM appValue
    root = do
      runReaderT reader env
        `catchError` ( \e -> do
                         logError e
                         throwError e
                     )
    logging = runStderrLoggingT root
    handler = withExceptT convert logging

app :: Env -> Application
app state = genericServeT (nt state) routesServer

logError :: MonadLogger m => Error -> m ()
logError (DatabaseError e) =
  logErrorN (T.pack $ show e)
logError (ObjectNotFoundError objId) =
  logInfoN $ T.concat ["Requested object not found: ", T.pack . show $ objId]
logError (LoopLinksForbidden) =
  logInfoN "Loop links are forbidden"
