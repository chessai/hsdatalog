{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Datalog.Graph where

import Control.Monad
import Control.Monad.Random.Strict (evalRandIO)
import Data.Foldable (toList)
import Data.List.Index (iforM_)
import Data.Map.Strict (Map)
import Data.Monoid (Sum(..))
import Data.Primitive.MutVar
import qualified Control.Monad.Random.Class as MonadRandom
import qualified Data.Graph as Containers
import qualified Data.List.Extra as Extra
import qualified Data.Map.Strict as Map

newtype Graph node weight
  = Graph
    { fromGraph :: Map node (Map node weight)
    }
  deriving (Show)

newGraph :: (Ord node) => Graph node weight
newGraph = Graph mempty

edges :: Graph node weight -> [(node, node, weight)]
edges = map (\(src, (tgt, w)) -> (src, tgt, w))
        . concatMap (\(src, tgts) -> (src,) <$> tgts)
        . Map.toList . fmap Map.toList . fromGraph

numEdges :: Graph node weight -> Int
numEdges = getSum . foldMap (Sum . Map.size) . fromGraph

vertices :: Graph node weight -> [node]
vertices = Map.keys . fromGraph

numVertices :: Graph node weight -> Int
numVertices = Map.size . fromGraph

neighbors :: (Ord node) => Graph node weight -> node -> [(node, weight)]
neighbors graph node = Map.toList $ Map.findWithDefault mempty node (fromGraph graph)

addVertex :: (Ord node) => node -> Graph node weight -> Graph node weight
addVertex node = Graph . Map.insertWith Map.union node mempty . fromGraph

addEdge :: (Ord node, Eq weight) => node -> node -> weight -> Graph node weight -> Graph node weight
addEdge source target weight =
  Graph . Map.insertWith Map.union source (Map.singleton target weight) . fromGraph

removeVertex :: (Ord node) => node -> Graph node weight -> Graph node weight
removeVertex node = Graph . fmap (Map.delete node) . Map.delete node . fromGraph

lookupEdge :: (Ord node) => node -> node -> Graph node weight -> Maybe weight
lookupEdge source target (Graph g) = Map.lookup source g >>= Map.lookup target

mapWeight
  :: (weight -> weight')
  -> Graph node weight
  -> Graph node weight'
mapWeight f = Graph . fmap (fmap f) . fromGraph

mapVertices
  :: (Ord node, Ord node')
  => (node -> node') -- ^ Must be a bijection!
  -> Graph node weight
  -> Graph node' weight
mapVertices f = Graph . Map.map (Map.mapKeys f) . Map.mapKeys f . fromGraph

sccGraph :: forall node weight. (Ord node) => Graph node weight -> Map node Int
sccGraph graph = Map.fromList (concatMap (\(i, ns) -> map (,i) ns) (zip [0..] scc))
  where
    scc :: [[node]]
    scc = sccListGraph graph

sccListGraph :: forall node weight. (Ord node) => Graph node weight -> [[node]]
sccListGraph (Graph graph) =
  map (map (intToVertices Map.!) . toList)
  $ Containers.scc
  $ Containers.buildG bounds
  $ concatMap (\(i, js) -> map (\(j, _) -> (i, j)) (Map.toList js))
  $ Map.toList
  $ fromGraph
  $ mapVertices (verticesToInt Map.!) (Graph graph)
  where
    bounds :: Containers.Bounds
    bounds = (0, Map.size graph - 1)

    verticesToInt :: Map node Int
    verticesToInt =
      Map.fromList $ zipWith (\(n, _) i -> (n, i)) (Map.toAscList graph) [0..]

    intToVertices :: Map Int node
    intToVertices =
      Map.fromList $ zipWith (\i (n, _) -> (i, n)) [0..] (Map.toAscList graph)

randomGraph :: Int -> Rational -> IO (Graph Int ())
randomGraph n frequencyOfOne = do
  m <- evalRandIO $ do
    flat <- replicateM (n ^ 2) (MonadRandom.fromList [(True, frequencyOfOne), (False, 1 - frequencyOfOne)])
    pure (Extra.chunksOf n flat)

  graphVar <- newMutVar newGraph

  iforM_ m $ \i xs -> do
    iforM_ xs $ \j b -> do
      modifyMutVar' graphVar (addVertex i . addVertex j)
      when b $ do
        when (i /= j) $ do
          modifyMutVar' graphVar (addEdge i j ())

  readMutVar graphVar
