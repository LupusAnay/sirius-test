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
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.Generics.Labels ()
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
nt env appValue = Handler $ servantValue
  where
    readerValue = runAppM appValue
    loggingValue = runReaderT readerValue env `catchError` errorHandler
    exceptValue = runStderrLoggingT loggingValue
    servantValue = withExceptT convert exceptValue
    errorHandler e = do
      logError e
      throwError e

app :: Env -> Application
app state = genericServeT (nt state) routesServer
