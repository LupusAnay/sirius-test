module Handlers
  ( listNodes,
    createNode,
    deleteNode,
    changeNodeLabel,
    listNeighbours,
    createLink,
  )
where

import Control.Monad.Except (MonadError)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Logger (MonadLogger, logInfoN)
import Data (Id, NewNode (..), Node (..))
import Database (MonadDB (..))
import qualified Database as DB
import Error
import Servant (NoContent (..))

listNodes :: (MonadDB m, MonadLogger m) => m [Node]
listNodes = do
  logInfoN "Getting Nodes"
  runSession DB.getNodes

createNode :: (MonadDB m) => NewNode -> m Id
createNode = runSession . DB.createNode

deleteNode :: (MonadDB m) => Id -> m NoContent
deleteNode nodeId = do
  _ <- runSession (DB.deleteNode nodeId) >>= maybeToNotFound nodeId
  pure NoContent

changeNodeLabel :: (MonadDB m) => Id -> NewNode -> m NoContent
changeNodeLabel nodeId newNode = do
  _ <- runSession (DB.updateNode nodeId newNode) >>= maybeToNotFound nodeId
  pure NoContent

listNeighbours :: (MonadDB m) => Id -> m [Node]
listNeighbours = runSession . DB.listNeighbours

createLink :: (MonadDB m) => Id -> Id -> m NoContent
createLink id1 id2
  | id1 == id2 = throwError LoopLinksForbidden
  | otherwise = do
    _ <- runSession (DB.createLink id1 id2) >>= maybeToNotFound id1
    pure NoContent

maybeToNotFound :: (MonadError Error m) => Id -> Maybe a -> m a
maybeToNotFound _ (Just a) = pure a
maybeToNotFound objId Nothing = throwError $ ObjectNotFoundError objId
