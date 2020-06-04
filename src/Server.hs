-- |
-- Module      : Server
-- Description : Contains API mappings and Wai application
module Server
  ( app,
  )
where

import Api
  ( GraphRoutes (..),
    LinkRoutes (..),
    NodeRoutes (..),
    Routes (..),
    api,
    apiWithSwagger,
  )
import App
import Control.Lens ((^.))
import Control.Monad.Except (MonadError (throwError), catchError, withExceptT)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Logger.CallStack (filterLogger)
import Control.Monad.Reader (runReaderT)
import Data.Generics.Labels ()
import Error
import qualified Handlers as H
import Servant ((:<|>) ((:<|>)), Application, Handler (..), hoistServer, serve)
import Servant.API.Generic (toServant)
import Servant.Server.Generic (AsServerT)
import Servant.Swagger.UI (swaggerSchemaUIServer)

-- | Servant mapping of handlers for nodes server
nodesServer :: NodeRoutes (AsServerT AppM)
nodesServer =
  NodeRoutes
    { listNodes = H.listNodes,
      createNode = H.createNode,
      deleteNode = H.deleteNode,
      changeLabel = H.changeNodeLabel,
      listNeighbours = H.listNeighbours
    }

-- | Servant mapping of handlers for links server
linksServer :: LinkRoutes (AsServerT AppM)
linksServer =
  LinkRoutes
    { createLink = H.createLink
    }

-- | Servant mapping of handlers for graph server
-- Unites nodes and links servers
graphServer :: GraphRoutes (AsServerT AppM)
graphServer =
  GraphRoutes
    { nodes = toServant nodesServer,
      links = toServant linksServer
    }

-- | Servant mapping for root server
routesServer :: Routes (AsServerT AppM)
routesServer =
  Routes
    { graph = toServant graphServer
    }

-- | Natural Transformer from AppM to Handler
nt :: Env -> AppM a -> Handler a
nt env appValue = Handler $ servantValue
  where
    readerValue = runAppM appValue
    loggingValue = runReaderT readerValue env `catchError` errorHandler
    exceptValue = runStderrLoggingT $ filterLogger loggerFilter loggingValue
    servantValue = withExceptT (convert logLevel) exceptValue
    logLevel = env ^. #config ^. #logLevel
    loggerFilter _ level = level >= logLevel
    errorHandler e = do
      logError e
      throwError e

-- | Wai App
app :: Env -> Application
app state = serve apiWithSwagger server'
  where
    graphServer' = hoistServer api (nt state) (toServant routesServer)
    swaggerServer' = swaggerSchemaUIServer graphSwagger
    server' = swaggerServer' :<|> graphServer'
