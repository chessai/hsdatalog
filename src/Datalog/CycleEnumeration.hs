{-# language ScopedTypeVariables #-}
{-# language TupleSections #-}
{-# language TypeApplications #-}

module Datalog.CycleEnumeration
  ( Graph
  , enumerateGraph
  , inducedSubgraph
  , randomGraph
  , graphToDot
  , scc
  ) where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Control.Monad.Trans.Writer.CPS (WriterT, execWriterT, tell)
import Control.Monad.Random.Strict (evalRandIO)
import Control.Monad.Random.Class (MonadRandom, getRandom)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.List.Index (imap, iconcatMap, iforM_)
import Data.Graph (Forest, Tree, buildG)
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Primitive.MutVar
import Data.Set (Set)
import Data.Vector.Mutable (MVector)
import qualified Control.Monad.Random.Class as MonadRandom
import qualified Data.Graph as Containers
import qualified Data.List as List
import qualified Data.List.Extra as Extra
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector

data Graph node weight = Graph
  { nodes :: [[(Int, weight)]]
  , toNode :: Map Int node
  , fromNode :: Map node Int
  }

emptyGraph :: Graph node weight
emptyGraph = Graph [] Map.empty Map.empty

index :: Int -> Graph node weight -> [(Int, weight)]
index v g = nodes g !! v

hasEdges :: Graph node weight -> Bool
hasEdges = any (not . null) . nodes

vertices :: Graph node weight -> [Int]
vertices g = [0 .. length (nodes g) - 1]

leastVertex :: [Int] -> Int
leastVertex = minimum

scc :: Graph node weight -> [[Int]]
scc = map toList . Containers.scc . graphToBadGraph . nodes
  where
    graphToBadGraph :: [[(Int, weight)]] -> Containers.Graph
    graphToBadGraph graph = buildG (0, length graph - 1) (concatMap go (zip [0..] graph))
      where
        go :: (Int, [(Int, weight)]) -> [Containers.Edge]
        go (src, targets) = map ((src,) . fst) targets

inducedSubgraph :: forall node weight. (Ord node) => Set node -> Graph node weight -> Graph node weight
inducedSubgraph ss graph = undefined
{-
  renameStep (map (filter (flip Set.member ss . fst) . ((nodes graph) !!)) ssList)
  where
    ssList = Set.toAscList ss

    renames :: Map Int Int
    renames = Map.fromList (zip ssList [0..])

    renameStep :: Graph node weight -> Graph node weight
    renameStep g = map (map (first specialLookup)) g

    specialLookup :: Int -> Int
    specialLookup x = Map.findWithDefault (error ("not in map: " ++ show x)) x renames
-}

-- | Compute the cycles in a 'WeightedGraph'
--
--   implements <https://www.cs.tufts.edu/comp/150GA/homeworks/hw1/Johnson%2075.PDF Johnson's Algorithm>
enumerateGraph :: forall node weight. (Ord node, Show weight) => Graph node weight -> [[node]]
enumerateGraph graph = runST $ execWriterT enumerate
  where
    enumerate :: forall s. WriterT [[node]] (ST s) ()
    enumerate = do
      let n = length graph
      _A <- newMutVar @_ @(Graph node weight) emptyGraph
      _B <- MVector.replicate @_ @(Set Int) n Set.empty
      blocked <- MVector.replicate @_ @Bool n False
      sVar <- newMutVar @_ @Int 0
      stackVar <- newMutVar @_ @[node] []
      let push x = modifyMutVar' stackVar (x :)
      let pop = modifyMutVar' stackVar tail

      let unblock :: Int -> WriterT [[node]] (ST s) ()
          unblock u = do
            MVector.write blocked u False
            bu <- MVector.read _B u
            forM_ bu $ \w -> do
              MVector.modify _B (Set.delete w) u
              bw <- MVector.read blocked w
              when bw (unblock w)

      let circuit :: Int -> WriterT [[node]] (ST s) Bool
          circuit v = do
            fVar <- newMutVar @_ @Bool False
            push v
            MVector.write blocked v True
            a <- (index v) <$> readMutVar _A
            forM_ a $ \(w, _) -> do
              s <- readMutVar sVar
              printST $ "w == " ++ show w
              printST $ "s == " ++ show s
              if w == s
                then do
                  cycle <- snoc <$> readMutVar stackVar <*> readMutVar sVar
                  tell [cycle]
                  writeMutVar fVar True
                else do
                  bw <- MVector.read blocked w
                  unless bw $ do
                    cw <- circuit w
                    when cw (writeMutVar fVar True)
            readMutVar fVar >>= \f -> if f
              then do
                unblock v
              else do
                forM_ a $ \(w, _) -> do
                  bw <- MVector.read _B w
                  unless (v `Set.member` bw) $ do
                    MVector.modify _B (Set.insert v) w
            pop
            readMutVar fVar

      let go :: WriterT [[node]] (ST s) ()
          go = do
            p <- readMutVar sVar >>= \s -> pure (s < n - 1)
            when p $ do
              _Ak <- do
                s <- readMutVar sVar
                -- The adjacency structure of strong component K with least
                -- vertex in subgraph of G induced by {s, s + 1, ..., n}
                let _Ak = id
                      $ flip inducedSubgraph graph
                      $ Set.fromList
                      $ fromMaybe (error "_Ak find failed")
                      $ List.find (s `elem`)
                      $ scc
                      $ inducedSubgraph (Set.fromList [s..n-1]) graph
                writeMutVar _A _Ak
                printST $ "_Ak = " ++ show _Ak
                pure _Ak
              let _Vk = vertices _Ak
              printST $ "hasEdges _Ak = " ++ show (hasEdges _Ak)
              if hasEdges _Ak
                then do
                  writeMutVar sVar (leastVertex _Vk)
                  forM_ _Vk $ \i -> do
                    MVector.write blocked i False
                    MVector.write _B i Set.empty
                  printST "Executing circuit"
                  readMutVar sVar >>= \s -> void (circuit s)
                  modifyMutVar' sVar (+1)
                else do
                  writeMutVar sVar n
      go

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

dup :: a -> (a, a)
dup a = (a, a)

printST :: PrimMonad m => String -> m ()
printST = unsafePrimToPrim . putStrLn
{-# noinline printST #-}

randomGraph :: Int -> Rational -> IO (Graph Int ())
randomGraph n frequencyOfOne = do
  m <- evalRandIO $ do
    flat <- replicateM (n ^ 2) (MonadRandom.fromList [(True, frequencyOfOne), (False, 1 - frequencyOfOne)])
    pure (Extra.chunksOf n flat)

  mv <- MVector.replicate @_ @[(Int, ())] n []

  iforM_ m $ \i xs -> do
    iforM_ xs $ \j b -> do
      when b $ do
        when (i /= j) $ do
          MVector.modify mv ((j, ()) :) i

  Vector.toList <$> Vector.unsafeFreeze mv

graphToDot :: Show node => Graph node weight -> String
graphToDot graph = iconcatMap (\i xs -> unlines (map (\(j, _) -> show i ++ " " ++ show j) xs)) graph

{-
  [ "digraph " ++ name ++ "{\n"
  , goVertices graph
  , goEdges graph
  , "}"
  ]
  where
    goVertices :: Graph weight -> String
    goVertices g = concatMap (indent 2 . (++ ";\n") . show) [0 .. length g - 1]

    goEdges :: Graph weight -> String
    goEdges = iconcatMap $ \i xs ->

    single :: Int -> (Int, weight) -> String
    single l (r, _) = indent 2 (show l ++ " -> " ++ show r ++ ";\n")

    indent :: Int -> String -> String
    indent n s = replicate n ' ' ++ s
-}
