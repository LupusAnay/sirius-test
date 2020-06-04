{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module : Database
-- Description : Contains Hasql sessions with decoders
module Database
  ( MonadDB (..),
    createNode,
    getNodes,
    listNeighbours,
    updateNode,
    nodeExists,
    linkExists,
    createLink,
    deleteNode,
  )
where

import Control.Lens ((^.))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data (Id, NewNode (..), Node (..))
import Data.Generics.Labels ()
import Data.Profunctor (dimap, lmap, rmap)
import qualified Data.Text as T
import qualified Data.Vector as V
import Error (Error (..))
import qualified Hasql.Session as HS
import Hasql.TH (maybeStatement, singletonStatement, vectorStatement)

-- | Database monad class. Used to run Hasql sessions
class (MonadIO m, MonadError Error m) => MonadDB m where
  -- | Accept session and return result
  runSession :: HS.Session a -> m a

  -- | Accept session and discard result
  runSession_ :: HS.Session a -> m ()

-- | Creates Node object from pair of id and label
nodeDecoder :: Integral a => (a, T.Text) -> Node
nodeDecoder (nId, nLabel) = Node {id = fromIntegral nId, label = nLabel}

-- | Create node session. Returning id of created object
createNode :: NewNode -> HS.Session Id
createNode node = HS.statement node statement
  where
    statement = dimap (^. #label) fromIntegral query
    query =
      [singletonStatement|
        insert into "nodes" (label) values ($1 :: text) returning id :: int4|]

-- | All nodes session. Return list of all nodes (no limit :( )
getNodes :: HS.Session [Node]
getNodes = HS.statement () statement
  where
    statement = rmap (V.toList . V.map nodeDecoder) query
    query =
      [vectorStatement|
        select n.id :: int4, n.label :: text from "nodes" n |]

-- | Delete node session. Return id of removed object if found removed
deleteNode :: Id -> HS.Session (Maybe Int)
deleteNode nodeId = HS.statement nodeId statement
  where
    statement = dimap fromIntegral (fmap fromIntegral) query
    query =
      [maybeStatement| delete from "nodes" where "id" = $1 :: int4
        returning "id" :: int4|]

-- | Update node label session. Return updated Node
updateNode :: Id -> NewNode -> HS.Session (Maybe Node)
updateNode nodeId node = HS.statement (nodeId, node) statement
  where
    encoder (_id, _node) = (fromIntegral _id, _node ^. #label)
    statement = dimap encoder (fmap nodeDecoder) query
    query =
      [maybeStatement|
        update "nodes" set "label" = $2 :: text where "id" = $1 :: int4
        returning id :: int4, label :: text|]

-- | Neighbours session. Returns all nodes connected to specified through links
listNeighbours :: Id -> HS.Session [Node]
listNeighbours nodeId = HS.statement nodeId statement
  where
    statement = dimap fromIntegral (V.toList . V.map nodeDecoder) query
    query =
      [vectorStatement|
        select n.id :: int4, n.label :: text from "nodes" n
        join "links" l on n.id = l.to_id where l.from_id = ($1 :: int4)
        union
        select n.id :: int4, n.label :: text from "nodes" n
        join "links" l on n.id = l.from_id where l.to_id = ($1 :: int4)|]

-- | Create link session. Returns Id of links row
createLink :: Id -> Id -> HS.Session Int
createLink id1 id2 = HS.statement (id1, id2) statement
  where
    encoder (_id1, _id2) = (fromIntegral id1, fromIntegral id2)
    statement = dimap encoder fromIntegral query
    query =
      [singletonStatement|
        insert into "links" (from_id, to_id) values ($1 :: int4, $2 :: int4)
        returning "id" :: int4|]

-- | Node exists session. Checks if node with id exists, returning Bool
nodeExists :: Id -> HS.Session Bool
nodeExists nodeId = HS.statement nodeId statement
  where
    statement = lmap fromIntegral query
    query =
      [singletonStatement|
        select exists(select 1 from nodes where id= $1 :: int4) :: bool|]

-- | Link exists session.
-- Checks if link between two nodes exists, returning Bool
linkExists :: Id -> Id -> HS.Session Bool
linkExists id1 id2 = HS.statement (id1, id2) statement
  where
    encoder (_id1, _id2) = (fromIntegral id1, fromIntegral id2)
    statement = lmap encoder query
    query =
      [singletonStatement| select (
        exists(select 1 from links
        where from_id = $1 :: int4 and to_id = $2 :: int4) or
        exists(select 1 from links
        where from_id = $2 :: int4 and to_id = $1 :: int4)) :: bool|]
