module Api
  ( Routes (..),
    GraphRoutes (..),
    LinkRoutes (..),
    NodeRoutes (..),
    api
  )
where

import Data (Id, NewNode, Node)
import GHC.Generics
import Servant
import Servant.API.Generic

data Routes route
  = Routes
      { graph :: route :- "graph" :> ToServant GraphRoutes AsApi
      }
  deriving (Generic)

data GraphRoutes route
  = GraphRoutes
      { nodes :: route :- "node" :> ToServant NodeRoutes AsApi,
        links :: route :- "link" :> ToServant LinkRoutes AsApi
      }
  deriving (Generic)

data LinkRoutes route
  = LinkRoutes
      { createLink ::
          route
            :- Capture "id_from" Id :> Capture "id_to" Id :> PutNoContent
      }
  deriving (Generic)

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

api :: Proxy (ToServantApi Routes)
api = genericApi (Proxy :: Proxy Routes)
