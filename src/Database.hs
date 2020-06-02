{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Database where

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
import Hasql.TH

class (MonadIO m, MonadError Error m) => MonadDB m where
  runSession :: HS.Session a -> m a

nodeDecoder :: Integral a => (a, T.Text) -> Node
nodeDecoder (nId, nLabel) = Node {id = fromIntegral nId, label = nLabel}

createNode :: NewNode -> HS.Session Id
createNode node = HS.statement node statement
  where
    statement = dimap (^. #label) fromIntegral query
    query =
      [singletonStatement|
        insert into "links" (label) values ($1 :: text) returning id :: int4|]

getNodes :: HS.Session [Node]
getNodes = HS.statement () statement
  where
    statement = rmap (V.toList . V.map nodeDecoder) query
    query =
      [vectorStatement|
        select "n.id" :: int4, "n.label" :: text from "nodes" n |]

deleteNode :: Id -> HS.Session ()
deleteNode nodeId = HS.statement nodeId statement
  where
    statement = lmap fromIntegral query
    query = [resultlessStatement| delete from "nodes" where "id" = $1 :: int4|]

updateNode :: Id -> NewNode -> HS.Session ()
updateNode nodeId node = HS.statement (nodeId, node) statement
  where
    encoder (_id, _node) = (fromIntegral _id, _node ^. #label)
    statement = lmap encoder query
    query =
      [singletonStatement|
        update "nodes" set "label" = $2 :: text where "id" = $1 :: int4|]

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
          join "links" l on n.id = l.from_id where l.to_id = ($1 :: int4)
        |]

createLink :: Id -> Id -> HS.Session ()
createLink id1 id2 = HS.statement (id1, id2) statement
  where
    encoder (_id1, _id2) = (fromIntegral id1, fromIntegral id2)
    statement = lmap encoder query
    query =
      [resultlessStatement|
          insert into "links" (from_id, to_id) values ($1 :: int4, $2 :: int4)|]
