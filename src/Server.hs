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
import Control.Monad.Except (withExceptT)
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
nt env appValue = Handler $ handler
  where
    reader = runAppM appValue
    root = runReaderT reader env
    handler = withExceptT convert root

app :: Env -> Application
app state = genericServeT (nt state) routesServer
