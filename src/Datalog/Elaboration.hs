{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Datalog.Elaboration where

import Control.Lens (Lens', (&), (%~), unsafePartsOf)
import Control.Monad
import Control.Monad.State.Class (get, modify)
import Control.Monad.State.Strict (State, evalState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.CPS (WriterT, tell)
import Data.Bifunctor (Bifunctor, bimap, first, second)
import Data.Data (Data)
import Data.Data.Lens
import Data.Either
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Typeable (Typeable)
import Datalog.RelAlgebra
import Datalog.Syntax

import qualified Data.List as List
import qualified Data.List.Extra as Extra
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

renameProgram
  :: forall rel var. (Ord var) => Program rel var -> Program rel Name
renameProgram = normaliseRules -- . removeSubgoalDuplication

normaliseRules
  :: forall rel var. (Ord var) => Program rel var -> Program rel Name
normaliseRules (Program ds ts) = Program (evalState (traverse go ds) 0) ts
  where
    go :: Declaration rel var -> State Int (Declaration rel Name)
    go d = do
      let (usedOnce, usedMany) = used d
      n <- get
      let renamings = Map.fromList
                      (map (,Nothing) usedOnce ++ zip usedMany (Just <$> [n..]))
      modify (+ length usedMany)
      pure (fmap (ElaborationName . (renamings Map.!)) d)

class Monad m => MonadTAC rel m | m -> rel where
  freshRel :: m rel
  freshVar :: m Name
  eqRel    :: m rel

data Rel = EqualityConstraint | Rel Int

newtype TacM a = TacM (State (Int, Int) a)
  deriving newtype (Functor, Applicative, Monad)

runTacM :: TacM a -> a
runTacM (TacM action) = evalState action (0, 0)

instance MonadTAC Rel TacM where
  freshRel = TacM $ do
    result <- fst <$> get
    modify (first (+1))
    pure (Rel result)

  freshVar = TacM $ do
    result <- snd <$> get
    modify (second (+1))
    pure (ElaborationName (Just result))

  eqRel = pure EqualityConstraint

instance (MonadTAC rel m) => MonadTAC rel (WriterT w m) where
  freshRel = lift freshRel
  freshVar = lift freshVar
  eqRel    = lift eqRel

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

isNameUnused :: Name -> Bool
isNameUnused = \case
  ParseName       m -> isNothing m
  ElaborationName m -> isNothing m

getVar :: Name -> Maybe Var
getVar = \case
  ParseName       _ -> Nothing
  ElaborationName m -> m

catVars :: [Name] -> [Var]
catVars = mapMaybe $ \case
  ParseName       _ -> Nothing
  ElaborationName m -> m

programToStatement
  :: forall m rel
  .  (MonadTAC rel m, Ord rel, Show rel)
  => Program rel Name
  -> m (Statement rel)
programToStatement (Program ds _) = fmap (Block . concat) (traverse go ds)
  where
    go :: Declaration rel Name -> m [Statement rel]
    go = undefined

removeSubgoalDuplication
  :: forall m rel
  .  (Data rel, MonadTAC rel m)
  => Declaration rel Name
  -> WriterT [Statement rel] m [Expr rel Name]
removeSubgoalDuplication (Rule _ ss) = impl ss
  where
    impl :: [Expr rel Name] -> WriterT [Statement rel] m [Expr rel Name]
    impl [] = pure []
    impl (subgoal : subgoals) = do
      let allVars :: Map Name Int
          allVars = Map.fromListWith (+) (zip (toList (fst subgoal)) (repeat 1))
      names <- traverse (`replicateM` freshVar) allVars
      let mkEquality (x, y) = do
            rel <- eqRel
            pure (Relation rel [Right x, Right y], NotNegated)
      equalities <- traverse mkEquality (concatMap sequenceA (Map.toList names))
      let modifyNames :: [Name] -> [Name]
          modifyNames = flip evalState Map.empty . traverse modifyName
            where
              modifyName :: Name -> State (Map Name Int) Name
              modifyName n = do
                let numUsages = Map.findWithDefault 0 n allVars
                if numUsages == 0
                  then pure n
                  else do
                    s <- get
                    let failure = error "modifyNames: encountered unseen name"
                    let ns = Map.findWithDefault failure n names
                    modify (Map.alter (Just . (+ 1) . fromMaybe 0) n)
                    pure (ns !! Map.findWithDefault 0 n s)

      let subgoal' :: Expr rel Name
          subgoal' = subgoal & temparts %~ modifyNames
      ((subgoal' : equalities) ++) <$> impl subgoals

temparts :: forall s a. (Data s, Typeable a) => Lens' s [a]
temparts = unsafePartsOf template

removeUnused
  :: (Monad m)
  => Declaration rel Name
  -> WriterT [Statement rel] m [Expr rel Name]
removeUnused d@(Rule _ subgoals) = do
  let allVars :: [(Name, Int)]
      allVars = Map.toAscList (Map.fromListWith (+) (zip (toList d) (repeat 1)))
  let usedOnce = map fst (filter ((== 1) . snd) allVars)
  let rename n = case n of
        ParseName       _ -> (n, ParseName       Nothing)
        ElaborationName _ -> (n, ElaborationName Nothing)
  let renamings = Map.fromList (map rename usedOnce)
  pure (map (first (fmap (renamings Map.!))) subgoals)

-- For each subgoal with an underscore, project away its unused attributes
projectUnused
  :: forall m rel
  .  (MonadTAC rel m)
  => Declaration rel Name
  -> WriterT [Statement rel] m [Expr rel Name]
projectUnused (Rule _ subgoals)
  | any isNameUnused (concatMap (toList . fst) subgoals)
  = flip zip (map snd subgoals) <$> traverse (go . fst) subgoals
  | otherwise
  = pure subgoals
  where
    go :: Relation rel Name -> WriterT [Statement rel] m (Relation rel Name)
    go (Relation rel vars)
      | not (any isUnused vars) = pure (Relation rel vars)
      | otherwise = do
          let positions = List.findIndices (not . isUnused) vars
          rel' <- freshRel
          tell [Assignment (TAC rel' (Project positions rel))]
          pure (Relation rel' (filter (not . isUnused) vars))

-- | For each subgoal with a constant, use the select and project operators to
--   restrict the relation to match the constant.
selectConstants
  :: forall m rel
  .  (MonadTAC rel m)
  => Declaration rel Name
  -> WriterT [Statement rel] m [Expr rel Name]
selectConstants (Rule _ subgoals) = flip zip (map snd subgoals)
                                    <$> traverse (go . fst) subgoals
  where
    go :: Relation rel Name -> WriterT [Statement rel] m (Relation rel Name)
    go (Relation rel args) | all isRight args = pure (Relation rel args)
    go (Relation rel args) = do
      let constants :: [(Int, Constant)]
          constants =
            rights $ map (sequenceA . fmap flipEither) $ zip [0..] args
      rels <- replicateM (length constants) freshRel
      let selects :: [Statement rel]
          selects = zipWith3 (\prevRel newRel (attr, constant) ->
                                Assignment
                                $ TAC newRel
                                $ Select attr constant prevRel)
                             (rel : init rels)
                             rels
                             constants
      tell selects
      projectRel <- freshRel
      tell [Assignment (TAC projectRel
                        (Project (List.findIndices isRight args) (last rels)))]

      pure (Relation projectRel (filter isRight args))

flipEither :: Either a b -> Either b a
flipEither = either Right Left

hasConstant :: Relation rel var -> Bool
hasConstant = any isLeft . relArguments

-- | Join each subgoal relation with each of the other subgoal relations,
--   projecting away attributes as they become unnecessary
--
-- TODO: lhs/rhs of join should be based on minimising renaming
--       could increase total amount of renaming but this is probably a good
--       greedy heuristic to adopt
joinSubgoals
  :: (MonadTAC rel m)
  => Declaration rel Name
  -> WriterT [Statement rel] m [Expr rel Name]
joinSubgoals (Rule _ subgoals) | not (needsJoin subgoals) = pure subgoals
joinSubgoals (Rule _ subgoals) = do
  renamedLHS <- freshRel
  tell [Assignment (TAC renamedLHS (Rename permutationLHS lhsRel))]
  renamedRHS <- freshRel
  tell [Assignment (TAC renamedRHS (Rename permutationRHS rhsRel))]
  joined <- freshRel
  tell [Assignment (TAC joined (Join (fromIntegral joinSize)
                                renamedLHS renamedRHS))]

  pure ((Relation joined (Right <$> joinedParams), NotNegated)
        : deleteAt lhs (deleteAt rhs subgoals))
  where
    variableUsedIn :: Map Var (Set SubgoalIndex)
    variableUsedIn =
      Map.fromListWith Set.union
      $ concatMap
        (\(ix, (Relation _ vars, _)) -> map (, Set.singleton ix)
                                        (catVars (rights vars)))
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
          joinIndices = map ((`unsafeElemIndex` names) . ElaborationName . Just)
                        $ toList joinVars
          notJoinIndices = map fst
                           $ filter (not . (`Set.member` joinVars) . snd)
                           $ mapMaybe (\(i, n) -> (i, ) <$> getVar n)
                           $ zip [0..] names
      in notJoinIndices ++ joinIndices
    permutationRHS =
      let names = rights rhsVars
          joinIndices = map ((`unsafeElemIndex` names) . ElaborationName . Just)
                        $ toList joinVars
          notJoinIndices = map fst
                           $ filter (not . (`Set.member` joinVars) . snd)
                           $ mapMaybe (\(i, n) -> (i, ) <$> getVar n)
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
needsJoin = any (> 1)
            . Map.fromListWith (+)
            . map (, 1 :: Int)
            . rights
            . concatMap (relArguments . fst)

applyPermutation :: AttrPermutation -> [a] -> [a]
applyPermutation perm xs = map (xs !!) perm

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
isUnused = either (const False) isNameUnused

mapBoth :: Bifunctor f => (a -> b) -> f a a -> f b b
mapBoth f = bimap f f

used :: forall t var. (Foldable t, Ord var) => t var -> ([var], [var])
used d =
  let allVars :: [(var, Int)]
      allVars = Map.toAscList (Map.fromListWith (+) (zip (toList d) (repeat 1)))
  in mapBoth (map fst) (List.partition ((== 1) . snd) allVars)

used' :: (Foldable t, Ord var) => t var -> Map Int [var]
used' d =
  let allVars = map (\(var, i) -> (i, [var]))
                $ Map.toAscList
                $ Map.fromListWith (+) (zip (toList d) (repeat 1))
  in Map.fromListWith (++) allVars

