-- |
-- Module : Api
-- Description : Contains Servant.API.Generic description of Graph API
module Api
  ( Routes (..),
    GraphRoutes (..),
    LinkRoutes (..),
    NodeRoutes (..),
    api,
    apiWithServant,
    ApiWithServant,
  )
where

import Data (Id, NewNode, Node)
import GHC.Generics
import Servant
import Servant.API.Generic
import Servant.Swagger.UI (SwaggerSchemaUI)

-- | Top level API data type
data Routes route
  = Routes
      { graph :: route :- "graph" :> ToServant GraphRoutes AsApi
      }
  deriving (Generic)

-- | Graph API data type
data GraphRoutes route
  = GraphRoutes
      { nodes :: route :- "node" :> ToServant NodeRoutes AsApi,
        links :: route :- "link" :> ToServant LinkRoutes AsApi
      }
  deriving (Generic)

-- | Link API data type
data LinkRoutes route
  = LinkRoutes
      { createLink ::
          route
            :- Capture "id_from" Id :> Capture "id_to" Id :> PutNoContent
      }
  deriving (Generic)

-- | Node API data type
data NodeRoutes route
  = NodeRoutes
      { listNodes :: route :- Get '[JSON] [Node],
        createNode :: route :- ReqBody '[JSON] NewNode :> Put '[JSON] Id,
        deleteNode :: route :- Capture "id" Id :> DeleteNoContent,
        changeLabel ::
          route
            :- Capture "id" Id :> ReqBody '[JSON] NewNode :> PutNoContent,
        listNeighbours ::
          route
            :- Capture "id" Id :> "neighbours" :> Get '[JSON] [Node]
      }
  deriving (Generic)

-- | Graph Api type united with swagger server
type ApiWithServant = SwaggerSchemaUI "swagger" "swagger.json" :<|> (ToServantApi Routes)

-- | Type proxy - phantom type info without construction of value.
-- Required for swagger
api :: Proxy (ToServantApi Routes)
api = genericApi (Proxy :: Proxy Routes)

-- | United type proxy. Required for Servant.serve
apiWithServant :: Proxy ApiWithServant
apiWithServant = Proxy
