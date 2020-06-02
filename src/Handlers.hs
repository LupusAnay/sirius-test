module Handlers
  ( listNodes,
    createNode,
    deleteNode,
    changeNodeLabel,
    listNeighbours,
    createLink,
  )
where

import Data (Id, NewNode (..), Node (..))
import Database (MonadDB (..))
import qualified Database as DB
import Servant (NoContent (..))

listNodes :: (MonadDB m) => m [Node]
listNodes = runSession DB.getNodes

createNode :: (MonadDB m) => NewNode -> m Id
createNode = runSession . DB.createNode

deleteNode :: (MonadDB m) => Id -> m NoContent
deleteNode nodeId = do
  runSession $ DB.deleteNode nodeId
  pure NoContent

changeNodeLabel :: (MonadDB m) => Id -> NewNode -> m NoContent
changeNodeLabel nodeId newNode = do
  runSession $ DB.updateNode nodeId newNode
  pure NoContent

listNeighbours :: (MonadDB m) => Id -> m [Node]
listNeighbours = runSession . DB.listNeighbours

createLink :: (MonadDB m) => Id -> Id -> m NoContent
createLink id1 id2 = do
  runSession $ DB.createLink id1 id2
  pure NoContent
