{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Datalog.Elaboration where

import Control.Monad
import Control.Monad.State.Class (get, modify, put)
import Control.Monad.State.Strict (State, evalState)
import Data.Bifunctor (Bifunctor, bimap, first, second)
import Data.Either
import Data.Foldable (foldlM, toList)
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Set (Set)
import Datalog.RelAlgebra
import Datalog.Syntax

import qualified Data.List as List
import qualified Data.List.Extra as Extra
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

used :: (Foldable t, Ord var) => t var -> ([var], [var])
used d =
  let allVars = Map.toAscList (Map.fromListWith (+) (zip (toList d) (repeat 1)))
  in mapBoth (map fst) (List.partition ((== 1) . snd) allVars)

renameProgram :: forall rel var. (Ord var) => Program rel var -> Program rel Name
renameProgram = normaliseRules . removeSubgoalDuplication

normaliseRules :: forall rel var. (Ord var) => Program rel var -> Program rel Name
normaliseRules (Program decls types) = Program (flip evalState 0 (traverse go decls)) types
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
removeSubgoalDuplication = fmap (,0)
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

data TAC rel
  = TAC rel (RelAlgebra rel)
  deriving stock (Eq, Ord, Show)

data Statement rel
  = While rel (Statement rel)
  | Block [Statement rel]
  | Assignment (TAC rel)
  deriving stock (Eq, Ord, Show)

class Monad m => MonadTAC rel m | m -> rel where
  freshRel :: m rel

type Rel = Int

newtype TacM a = TacM (State Int a)
  deriving newtype (Functor, Applicative, Monad)

runTacM :: TacM a -> a
runTacM (TacM action) = evalState action 0

instance MonadTAC Rel TacM where
  freshRel = TacM $ do
    result <- get
    modify (+1)
    pure result

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

type SubgoalIndex = Int

programToStatement
  :: forall m rel
  .  (MonadTAC rel m, Ord rel, Show rel)
  => Program rel Name
  -> m (Statement rel)
programToStatement (Program decls _) = fmap (Block . concat) (traverse go decls)
  where
    go :: Declaration rel Name -> m [Statement rel]
    -- For each subgoal with an underscore, project away its unused attributes
    go (Rule relH exprs) | any isNothing (concatMap (toList . fst) exprs) = do
      (statements, exprs') <- (bimap concat (flip zip (map snd exprs)) . unzip)
                              <$> traverse (projectUnused . fst) exprs
      (statements ++) <$> go (Rule relH exprs')
      -- FIXME: wrong because we need two different namespaces for
      -- compiler-generated vs parsed names
    -- Join each subgoal relation with each of the other subgoal relations,
    -- projecting away attributes as they become unnecessary
    go decl = error (show decl)

    projectUnused :: Relation rel Name -> m ([Statement rel], Relation rel Name)
    projectUnused (Relation rel vars)
      | all (not . isUnused) vars = pure ([], Relation rel vars)
      | otherwise = do
          let positions = List.findIndices (not . isUnused) vars
          rel' <- freshRel
          pure ( [Assignment (TAC rel' (Project positions rel))]
               , Relation rel' (filter (not . isUnused) vars)
               )

    joinSubgoals :: [Expr rel Name] -> m ([Statement rel], [Expr rel Name])
    joinSubgoals subgoals | not (needsJoin subgoals) = pure ([], subgoals)
    joinSubgoals subgoals = do
      renamedLHS <- freshRel
      renamedRHS <- freshRel
      joined <- freshRel
      pure ( [ Assignment (TAC renamedLHS (Rename permutationLHS lhsRel))
             , Assignment (TAC renamedRHS (Rename permutationRHS rhsRel))
             , Assignment (TAC joined (Join (fromIntegral joinSize)
                                       renamedLHS renamedRHS))
             ]
           , (Relation joined (Right <$> joinedParams), NotNegated)
             : deleteAt lhs (deleteAt rhs subgoals)
           )
      where
        variableUsedIn :: Map Var (Set SubgoalIndex)
        variableUsedIn =
          Map.fromListWith Set.union
          $ concatMap
            (\(ix, (Relation _ vars, _)) -> map (, Set.singleton ix)
                                            (catMaybes (rights vars)))
          $ zip [0..] subgoals

        joinPairs :: Map (SubgoalIndex, SubgoalIndex) (Set Var)
        joinPairs =
          Map.fromListWith Set.union
          $ concatMap (\(var, ps) -> (, Set.singleton var) <$> Set.toList ps)
          $ Map.toList (pairsOf <$> variableUsedIn)

        lhs, rhs :: SubgoalIndex
        joinVars :: Set Var
        ((lhs, rhs), joinVars) = Extra.maximumOn (Set.size . snd)
                                 $ Map.toList joinPairs

        Relation lhsRel lhsVars = fst (subgoals !! lhs)
        Relation rhsRel rhsVars = fst (subgoals !! rhs)

        joinSize = Set.size joinVars

        permutationLHS, permutationRHS :: AttrPermutation
        permutationLHS =
          let names = rights lhsVars
              joinIndices = map ((`unsafeElemIndex` names) . Just)
                            $ toList joinVars
              notJoinIndices = map fst
                               $ filter (not . (`Set.member` joinVars) . snd)
                               $ mapMaybe sequenceA
                               $ zip [0..] names
          in notJoinIndices ++ joinIndices
        permutationRHS =
          let names = rights rhsVars
              joinIndices = map ((`unsafeElemIndex` names) . Just)
                            $ toList joinVars
              notJoinIndices = map fst
                               $ filter (not . (`Set.member` joinVars) . snd)
                               $ mapMaybe sequenceA
                               $ zip [0..] names
          in joinIndices ++ notJoinIndices

        unsafeElemIndex :: Eq a => a -> [a] -> Int
        unsafeElemIndex el xs = fromMaybe undefined -- FIXME
                                (List.elemIndex el xs)

        joinedParams :: [Name]
        joinedParams =
          applyPermutation permutationLHS (rights lhsVars)
          ++ drop joinSize (applyPermutation permutationRHS (rights rhsVars))

    needsJoin :: [Expr rel Name] -> Bool
    needsJoin = undefined -- FIXME

applyPermutation :: AttrPermutation -> [a] -> [a]
applyPermutation = undefined

pairsOf :: (Ord a) => Set a -> Set (a, a)
pairsOf vals = Set.fromList
               [ sortPair (x, y)
               | x <- Set.toList vals
               , y <- Set.toList vals
               , x /= y
               ]
  where
    sortPair (a, b) = if a < b then (a, b) else (b, a)

deleteAt :: Int -> [a] -> [a]
deleteAt n xs = uncurry (++) $ second tail $ Extra.splitAt n xs

isUnused :: Either Constant Name -> Bool
isUnused = either (const False) isNothing

mapBoth :: Bifunctor f => (a -> b) -> f a a -> f b b
mapBoth f = bimap f f
