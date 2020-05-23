{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Datalog.CycleEnumeration
  ( enumerateCycles
  , enumerateWeightCycles
  ) where

import           Control.Exception              (assert)
import           Control.Monad
import           Control.Monad.Extra
import           Control.Monad.Primitive
import           Control.Monad.Random.Class     (MonadRandom, getRandom)
import qualified Control.Monad.Random.Class     as MonadRandom
import           Control.Monad.Random.Strict    (evalRandIO)
import           Control.Monad.ST               (ST, runST)
import           Control.Monad.ST.Unsafe        (unsafeIOToST)
import           Control.Monad.Trans.Writer.CPS (WriterT, execWriterT, tell)
import           Data.Bifunctor                 (first, second)
import           Data.Foldable                  (toList, traverse_)
import           Data.Graph                     (Forest, Tree, buildG)
import qualified Data.Graph                     as Containers
import qualified Data.List                      as List
import qualified Data.List.Extra                as Extra
import           Data.List.Index                (iconcatMap, iforM_, imap)
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import           Data.Maybe
import           Data.Monoid                    (Sum (..))
import           Data.Primitive.MutVar
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import qualified Data.Vector                    as Vector
import           Data.Vector.Mutable            (MVector)
import qualified Data.Vector.Mutable            as MVector

import           Datalog.Graph

newtype Table s k v = Table (MutVar s (Map k v))

newTable :: (PrimMonad m, Ord k) => m (Table (PrimState m) k v)
newTable = Table <$> newMutVar mempty

insertTable :: (PrimMonad m, Ord k) => Table (PrimState m) k v -> k -> v -> m ()
insertTable (Table t) k v = modifyMutVar' t (Map.insert k v)

lookupTable :: (PrimMonad m, Ord k) => Table (PrimState m) k v -> k -> m (Maybe v)
lookupTable (Table t) k = Map.lookup k <$> readMutVar t

findWithDefaultTable :: (PrimMonad m, Ord k) => Table (PrimState m) k v -> v -> k -> m v
findWithDefaultTable (Table t) def k = Map.findWithDefault def k <$> readMutVar t

deleteTable :: (PrimMonad m, Ord k) => Table (PrimState m) k v -> k -> m ()
deleteTable (Table t) k = modifyMutVar' t (Map.delete k)

modifyTable :: (PrimMonad m, Ord k) => Table (PrimState m) k v -> k -> (v -> v) -> m ()
modifyTable (Table t) k f = modifyMutVar' t (Map.adjust f k)

enumerateWeightCycles :: forall node weight. (Ord node) => Graph node weight -> [[weight]]
enumerateWeightCycles graph = map (\loop -> go (loop ++ [head loop])) simpleCycles
  where
    go :: [node] -> [weight]
    go [] = []
    go [x] = []
    go (x : y : rest) = fromMaybe (error "enumerateWeightCycles: invariant violation") (lookupEdge x y graph) : go (y : rest)

    simpleCycles = enumerateCycles graph

-- | Compute the cycles in a 'WeightedGraph'
--
--   implements <https://www.cs.tufts.edu/comp/150GA/homeworks/hw1/Johnson%2075.PDF Johnson's Algorithm>
enumerateCycles :: forall node weight. (Ord node) => Graph node weight -> [[node]]
enumerateCycles (mapWeight (const ()) -> graph) = runST impl
  where
    impl :: forall s. ST s [[node]]
    impl = do
      pathVar    <- newMutVar @_ @[node] []
      blockedVar <- newTable  @_ @node @Bool
      bVar       <- newTable  @_ @node @[node]
      resultVar  <- newMutVar @_ @[[node]] []

      let getBlocked :: node -> ST s Bool
          getBlocked = findWithDefaultTable blockedVar False
          setBlocked :: node -> Bool -> ST s ()
          setBlocked = insertTable blockedVar

      let getB :: node -> ST s [node]
          getB = findWithDefaultTable bVar []
          setB :: node -> [node] -> ST s ()
          setB = insertTable bVar

      let push :: node -> ST s ()
          push n = modifyMutVar' pathVar (n:)
          pop :: ST s ()
          pop = modifyMutVar' pathVar tail

      let unblock :: node -> ST s ()
          unblock n = do
            b <- getBlocked n
            when b $ do
              setBlocked n False
              getB n >>= traverse_ unblock
              setB n []

      let circuit :: node -> node -> Graph node () -> ST s Bool
          circuit thisNode startNode component = do
            closedVar <- newMutVar False
            push thisNode
            setBlocked thisNode True
            let neighbors_ = neighbors component thisNode
            forM_ neighbors_ $ \(nextNode, _) -> do
              if nextNode == startNode
                then do path <- readMutVar pathVar
                        modifyMutVar' resultVar (path:)
                        writeMutVar closedVar True
                else do unlessM (getBlocked nextNode) $ do
                          whenM (circuit nextNode startNode component) $ do
                            writeMutVar closedVar True
            closed <- readMutVar closedVar
            if closed
              then do
                unblock thisNode
              else do
                forM_ neighbors_ $ \(nextNode, _) -> do
                  l <- getB nextNode
                  unless (thisNode `elem` l) $ do
                    setB nextNode (thisNode : l)
            pop
            pure closed

      let extractSubgraph :: [node] -> Graph node () -> Graph node ()
          extractSubgraph s g = runST $ do
            sgVar <- newMutVar newGraph
            forM_ s $ \v1 -> do
              modifyMutVar' sgVar (addVertex v1)
              forM_ (neighbors g v1) $ \(v2, _) -> do
                when (v2 `elem` s) $ do
                  modifyMutVar' sgVar (addEdge v1 v2 ())
            readMutVar sgVar

      let sccWithVertex :: node -> Graph node () -> Graph node ()
          sccWithVertex v g = runST $ do
            let scc = sccGraph g
            let n = scc Map.! v
            sgVar <- newMutVar newGraph
            forM_ (vertices g) $ \v1 -> do
              when (scc Map.! v1 == n) $ do
                forM_ (neighbors g v1) $ \(v2, _) -> do
                  when (scc Map.! v2 == n) $ do
                    modifyMutVar' sgVar (addEdge v1 v2 ())
            readMutVar sgVar

      let nonDegenerateSCC :: [[node]]
          nonDegenerateSCC = filter (\s -> length s > 1) (sccListGraph graph)

      let nonDegenerateSubgraphs :: [Graph node ()]
          nonDegenerateSubgraphs = map (`extractSubgraph` graph) (List.sort nonDegenerateSCC)

      forM_ nonDegenerateSubgraphs $ \subgraph -> do
        let go :: Graph node () -> [node] -> ST s ()
            go _ [] = pure ()
            go g (s:rest) = do
              let component = sccWithVertex s g
              when (numEdges component > 0) $ do
                forM_ (vertices component) $ \node -> do
                  setBlocked node False
                  setB node []
                void $ circuit s s component
              go (removeVertex s g) rest

        go subgraph (List.sort (vertices subgraph))

      map reverse . reverse <$> readMutVar resultVar
