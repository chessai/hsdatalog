{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Datalog.Elaboration where

import           Control.Monad
import           Control.Monad.State.Class  (get, modify, put)
import           Control.Monad.State.Strict (State, evalState)
import           Data.Bifunctor             (Bifunctor, bimap, first, second)
import           Data.Either
import           Data.Foldable              (foldlM, toList)
import qualified Data.List                  as List
import qualified Data.List.Extra            as Extra
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Datalog.RelAlgebra
import           Datalog.Syntax


used :: (Foldable t, Ord var) => t var -> ([var], [var])
used d =
  let allVars = Map.toAscList (Map.fromListWith (+) (zip (toList d) (repeat 1)))
  in mapBoth (map fst) (List.partition ((== 1) . snd) allVars)

renameProgram :: forall rel var. (Ord var) => Program rel var -> Program rel Name
renameProgram = normaliseRules . removeSubgoalDuplication

normaliseRules :: forall rel var. (Ord var) => Program rel var -> Program rel Name
normaliseRules = flip evalState 0 . traverse go
  where
    go :: Declaration rel var -> State Int (Declaration rel Name)
    go d = do
      let (usedOnce, usedMany) = used d
      n <- get
      let renamings = Map.fromList (map (,Nothing) usedOnce ++ zip usedMany (Just <$> [n..]))
      modify (+ length usedMany)
      pure (fmap (renamings Map.!) d)

-- FIXME: make this do its damn JOB!
removeSubgoalDuplication :: forall rel var. (Ord var) => Program rel var -> Program rel (var, Int)
removeSubgoalDuplication = map (fmap (,0))
{-
  map go
  where
    go :: Declaration rel var -> Declaration rel (var, Int)
    go (Rule relH exprs) = Rule ((,0) <$> relH) (concat (evalState (traverse foo exprs) 1))
      --let (usedOnce, usedMany) = used exprs
      --in undefined

    foo :: Expr rel var -> State Int [Expr rel (var, Int)]
    foo (Relation rel vars, b) = do
      let (usedOnce, usedMany) = used vars
      n <- get
      undefined
-}

data TAC rel = TAC rel (RelAlgebra rel)

data Statement rel
  = While rel (Statement rel)
  | Block [Statement rel]
  | Assignment (TAC rel)

class Monad m => MonadTAC rel m | m -> rel where
  freshRel :: m rel

-- First, construct a map whose keys are the names of relations in the subgoals
-- of a rule, and whose values are the set of variables used in those relations.
--
-- Then, invert it. Now we have a map from variables to the set of relations in
-- which they are used.
--
-- Then, map over the values with a function that enumerates all possible pairs
-- that can be produced from the set. Note that pairs are unordered and must
-- have different elements (i.e. sets of size 2).
--
-- Finally, invert the map again to get a map from pairs of relations to sets of
-- variables. Choose the pair of relations that has the largest number of
-- variables, and join on those variables. Then replace all uses of those two
-- relations in the map in step 2 (if renaming results in a pair that has the
-- same relation twice is, the pair should be removed). Repeat this process
-- until there are no more pairs left.
programToStatement :: forall m rel. MonadTAC rel m => Program rel Name -> m (Statement rel)
programToStatement = fmap (Block . concat) . traverse go
  where
    go :: Declaration rel Name -> m [Statement rel]
    -- For each subgoal with an underscore, project away its unused attributes
    go (Rule relation exprs) | any isNothing (concatMap (toList . fst) exprs) = do
      (statements, exprs') <- (bimap concat (flip zip (map snd exprs)) . unzip)
                              <$> traverse (projectUnused . fst) exprs
      (statements ++) <$> go (Rule relation exprs')
    -- Join each subgoal relation with each of the other subgoal relations,
    -- projecting away attributes as they become unnecessary

    projectUnused :: Relation rel Name -> m ([Statement rel], Relation rel Name)
    projectUnused (Relation rel vars)
      | all (not . isUnused) vars = pure ([], Relation rel vars)
      | otherwise = do
          let positions = List.findIndices (not . isUnused) vars
          rel' <- freshRel
          pure ([Assignment (TAC rel' (Project positions rel))], Relation rel' (filter (not . isUnused) vars))


isUnused :: Either Constant Name -> Bool
isUnused = either (const False) isNothing

mapBoth :: Bifunctor f => (a -> b) -> f a a -> f b b
mapBoth f = bimap f f
