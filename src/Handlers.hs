-- |
-- Module : Handlers
-- Description : Servant Request handlers
module Handlers
  ( listNodes,
    createNode,
    deleteNode,
    changeNodeLabel,
    listNeighbours,
    createLink,
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Logger (MonadLogger, logInfoN)
import Data (Id, NewNode (..), Node (..))
import Database (MonadDB (..))
import qualified Database as DB
import Error
import Servant (NoContent (..))

-- | List all nodes handler. 
-- Returns list of nodes
listNodes :: (MonadDB m, MonadLogger m) => m [Node]
listNodes = do
  logInfoN "Getting Nodes"
  runSession DB.getNodes

-- | Create node handler. 
-- Accepts NewNode object {"label": "Example Label"}
-- Returning Id of created Node 
createNode :: (MonadDB m) => NewNode -> m Id
createNode = runSession . DB.createNode

-- | Delete node handler
-- Accepts Id of node to delete
-- Returning NoContent
-- Fails with 404 if node does not exist
deleteNode :: (MonadDB m) => Id -> m NoContent
deleteNode nodeId = do
  _ <- runSession (DB.deleteNode nodeId) >>= maybeToNotFound nodeId
  pure NoContent

-- | Update node handler
-- Accepts Id of node to change and new label
-- Returning NoContent
-- Fails with 404 if node does not exist
changeNodeLabel :: (MonadDB m) => Id -> NewNode -> m NoContent
changeNodeLabel nodeId newNode = do
  _ <- runSession (DB.updateNode nodeId newNode) >>= maybeToNotFound nodeId
  pure NoContent

-- | List neighbours handler
-- Accepts Id of node
-- Returning list of Nodes
-- Fails with 404 if node does not exist
listNeighbours :: (MonadDB m) => Id -> m [Node]
listNeighbours nodeId = do
  exists <- runSession $ DB.nodeExists nodeId
  when (not exists) (throwError $ ObjectNotFoundError nodeId)
  runSession $ DB.listNeighbours nodeId

-- | Create link handler
-- Accepts ids of nodes to connect
-- Returning NoContent
-- Fails if one of nodes does not exist or if link already created
createLink :: (MonadDB m) => Id -> Id -> m NoContent
createLink id1 id2
  | id1 == id2 = throwError LoopLinksForbidden
  | otherwise = do
    firstExist <- runSession $ DB.nodeExists id1
    secondExist <- runSession $ DB.nodeExists id2
    when (not firstExist) (throwError $ ObjectNotFoundError id1)
    when (not secondExist) (throwError $ ObjectNotFoundError id2)
    linkExists <- runSession $ DB.linkExists id1 id2
    when (linkExists) (throwError $ LinkAlreadyExists id1 id2)
    runSession_ (DB.createLink id1 id2)
    pure NoContent

-- | Helper function. 
-- Accepts Maybe and Id and throws NotFound Error if Maybe is Nothing
maybeToNotFound :: (MonadError Error m) => Id -> Maybe a -> m a
maybeToNotFound _ (Just a) = pure a
maybeToNotFound objId Nothing = throwError $ ObjectNotFoundError objId
