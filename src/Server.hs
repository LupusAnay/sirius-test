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
    SwaggerApi,
    swaggerApi,
  )
import App
import Control.Lens ((^.))
import Control.Monad.Except
  ( MonadError (throwError),
    catchError,
    liftEither,
    withExceptT,
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Logger.CallStack (filterLogger)
import Control.Monad.Reader (runReaderT)
import Data.Either.Combinators (mapLeft)
import Data.Generics.Labels ()
import Error
import qualified Handlers as H
import Servant
  ( Application,
    Handler (..),
    ServerT,
    hoistServer,
    runHandler,
  )
import Servant.API.Generic (toServant)
import Servant.Server.Generic (AsServerT, genericServeT)
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

swaggerServ :: ServerT SwaggerApi AppM
swaggerServ =
  hoistServer swaggerApi (ntSwagger) (swaggerSchemaUIServer graphSwagger)

-- | Servant mapping for root server
routesServer :: Routes (AsServerT AppM)
routesServer =
  Routes
    { graph = toServant graphServer,
      swagger = swaggerServ
    }

ntSwagger :: Handler a -> AppM a
ntSwagger sw = do
  fuck <- liftIO $ runHandler sw
  liftEither $ (mapLeft ServantError) fuck

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
app state = genericServeT (nt state) routesServer
