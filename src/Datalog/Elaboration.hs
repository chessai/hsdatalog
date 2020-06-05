--------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UndecidableInstances       #-}

--------------------------------------------------------------------------------

module Datalog.Elaboration where

--------------------------------------------------------------------------------

import Control.Lens (Lens', unsafePartsOf, (%~), (&))
import Control.Monad
import Control.Monad.Fix (mfix)
import Control.Monad.State.Class (get, modify)
import Control.Monad.State.Strict (State, evalState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.CPS (WriterT, execWriterT, tell)
import Data.Bifunctor (Bifunctor, bimap, first, second)
import Data.Bits (finiteBitSize, testBit)
import Data.Data (Data)
import Data.Data.Lens
import Data.Either
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Typeable (Typeable)
import Datalog.Pretty
import Datalog.RelAlgebra
import Datalog.Syntax
import Debug.Trace

import qualified Data.List as List
import qualified Data.List.Extra as Extra
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

--------------------------------------------------------------------------------

class Monad m => MonadTAC rel m | m -> rel where
  freshRel :: m rel
  freshVar :: m Name
  eqRel    :: m rel

newtype TacM a = TacM (State (Int, Int) a)
  deriving newtype (Functor, Applicative, Monad)

runTacM :: TacM a -> a
runTacM (TacM action) = evalState action (0, 0)

instance MonadTAC Rel TacM where
  freshRel = TacM $ do
    result <- fst <$> get
    modify (first (+1))
    pure (ElaborationRel result)

  freshVar = TacM $ do
    result <- snd <$> get
    modify (second (+1))
    pure (ElaborationName (Just result))

  eqRel = pure EqualityConstraint

instance (MonadTAC rel m) => MonadTAC rel (WriterT w m) where
  freshRel = lift freshRel
  freshVar = lift freshVar
  eqRel    = lift eqRel

type SubgoalIndex = Int

--------------------------------------------------------------------------------

programToStatement
  :: Program Rel Name
  -> Statement Rel
programToStatement (Program ds _) = runTacM $ do
  statement <- (Block . map Block) <$> traverse iWantItAll ds
  let isParseRel :: Rel -> Bool
      isParseRel (ParseRel _) = True
      isParseRel _ = False
  pure (While (filter isParseRel (setNub (toList statement))) statement)

--------------------------------------------------------------------------------

-- TODO: I think this function is probably unnecessary, we should preserve
-- ParseNames for as long as we possibly can.
renameProgram
  :: forall rel var. (Ord var) => Program rel var -> Program rel Name
renameProgram (Program ds ts) = Program (evalState (traverse go ds) 0) ts
  where
    go :: Declaration rel var -> State Int (Declaration rel Name)
    go d = do
      let (usedOnce, usedMany) = used d
      n <- get
      let renamings = Map.fromList
                      (map (,Nothing) usedOnce ++ zip usedMany (Just <$> [n..]))
      modify (+ length usedMany)
      pure (fmap (ElaborationName . (renamings Map.!)) d)

--------------------------------------------------------------------------------

iWantItAll
  :: forall m rel
  . (Data rel, Eq rel, MonadTAC rel m)
  => Declaration rel Name
  -> m [Statement rel]
iWantItAll d = execWriterT $ do
  unifyHeadRelation =<< flip mcompose d
    [ canoniseConstants
    , removeDuplication
    , eliminateNegation
    , removeUnused
    , projectUnused
    , selectConstants
    , fixpoint joinSubgoals
    , generateEverythings
    , joinEverythingElse
    , renameToHead
    ]

mcompose :: (Foldable t, Monad m) => t (a -> m a) -> a -> m a
mcompose = foldr (>=>) pure

fixpoint :: (Eq a, Monad m) => (a -> m a) -> a -> m a
fixpoint f x0 = do
  x <- f x0
  if x == x0
    then pure x
    else fixpoint f x

--------------------------------------------------------------------------------

-- remove duplication in the head relation and subgoals
removeDuplication
  :: forall m rel
  .  (Data rel, MonadTAC rel m)
  => Declaration rel Name
  -> WriterT [Statement rel] m (Declaration rel Name)
removeDuplication (Rule relH ss) = do
  exprs <- impl ((relH, NotNegated) : ss)
  case exprs of
    ((r, NotNegated) : es) -> pure (Rule r es)
    _ -> error "removeDuplication: whaaaaaaa?"
  where
    impl :: [Expr rel Name] -> WriterT [Statement rel] m [Expr rel Name]
    impl [] = pure []
    impl (subgoal : subgoals) = do
      let allVars :: Map Name Int
          allVars =
            Map.fromList
            $ filter ((> 1) . snd)
            $ Map.toList
            $ Map.fromListWith (+) (zip (toList (fst subgoal)) (repeat 1))
      names <- traverse (`replicateM` freshVar) allVars

      let mkEquality (x, y) = do
            rel <- eqRel
            pure (Relation rel [Right x, Right y], NotNegated)
      equalities <- traverse mkEquality (concatMap sequenceA (Map.toList names))

      let modifyName :: Name -> State (Map Name Int) Name
          modifyName n = do
            if Map.findWithDefault 0 n allVars <= 1
              then pure n
              else do
                s <- get
                let failure = error "modifyNames: encountered unseen name"
                let ns = Map.findWithDefault failure n names
                modify (Map.alter (Just . (+ 1) . fromMaybe 0) n)
                pure (ns !! Map.findWithDefault 0 n s)
      let modifyNames :: [Name] -> [Name]
          modifyNames = flip evalState Map.empty . traverse modifyName
      let subgoal' :: Expr rel Name
          subgoal' = subgoal & temparts %~ modifyNames

      ((subgoal' : equalities) ++) <$> impl subgoals

--------------------------------------------------------------------------------

-- | Eliminate all uses of negation from the given 'Declaration'.
eliminateNegation
  :: forall m rel
  .  (MonadTAC rel m)
  => Declaration rel Name
  -> WriterT [Statement rel] m (Declaration rel Name)
eliminateNegation (Rule relH subgoals) = Rule relH <$> traverse go subgoals
  where
    go :: Expr rel Name -> WriterT [Statement rel] m (Expr rel Name)
    go (relation, NotNegated) = pure (relation, NotNegated)
    go (Relation rel args, Negated) = do
      nrel <- freshRel
      tell [Assignment (TAC nrel (Not rel))]
      pure (Relation nrel args, NotNegated)

--------------------------------------------------------------------------------

-- | Replace variable names that are only used once with 'Nothing'.
removeUnused
  :: (Monad m)
  => Declaration rel Name
  -> WriterT [Statement rel] m (Declaration rel Name)
removeUnused d@(Rule relH subgoals) = do
  let allVars :: [(Name, Int)]
      allVars = Map.toAscList (Map.fromListWith (+) (zip (toList d) (repeat 1)))
  let usedOnce = map fst (filter ((== 1) . snd) allVars)
  let rename n = case n of
        ParseName       _ -> (n, ParseName       Nothing)
        ElaborationName _ -> (n, ElaborationName Nothing)
  let renamings = Map.fromList (map rename usedOnce)
  pure (Rule relH (map (first (fmap (\key -> Map.findWithDefault key key renamings))) subgoals))

--------------------------------------------------------------------------------

-- | For each subgoal with an underscore, project away its unused attributes.
projectUnused
  :: forall m rel
  .  (MonadTAC rel m)
  => Declaration rel Name
  -> WriterT [Statement rel] m (Declaration rel Name)
projectUnused (Rule relH subgoals)
  | any isNameUnused (concatMap (toList . fst) subgoals)
  = (Rule relH . flip zip (map snd subgoals)) <$> traverse (go . fst) subgoals
  | otherwise
  = pure (Rule relH subgoals)
  where
    go :: Relation rel Name -> WriterT [Statement rel] m (Relation rel Name)
    go (Relation rel vars)
      | not (any isUnused vars) = pure (Relation rel vars)
      | otherwise = do
          let positions = List.findIndices (not . isUnused) vars
          rel' <- freshRel
          tell [Assignment (TAC rel' (Project positions rel))]
          pure (Relation rel' (filter (not . isUnused) vars))

--------------------------------------------------------------------------------

-- | For each subgoal with a constant, use the select and project operators to
--   restrict the relation to match the constant.
selectConstants
  :: forall m rel
  .  (MonadTAC rel m)
  => Declaration rel Name
  -> WriterT [Statement rel] m (Declaration rel Name)
selectConstants (Rule relH subgoals) = (Rule relH . flip zip (map snd subgoals))
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

--------------------------------------------------------------------------------

-- | Join each subgoal relation with each of the other subgoal relations,
--   projecting away attributes as they become unnecessary.
--
-- TODO: lhs/rhs of join should be based on minimising renaming
--       could increase total amount of renaming but this is probably a good
--       greedy heuristic to adopt
joinSubgoals
  :: (MonadTAC rel m)
  => Declaration rel Name
  -> WriterT [Statement rel] m (Declaration rel Name)
joinSubgoals (Rule relH subgoals) | not (needsJoin subgoals) = pure (Rule relH subgoals)
joinSubgoals d@(Rule relH subgoals) = do
  renamedLHS <- freshRel
  tell [Assignment (TAC renamedLHS (Rename permutationLHS lhsRel))]
  renamedRHS <- freshRel
  tell [Assignment (TAC renamedRHS (Rename permutationRHS rhsRel))]
  joined <- freshRel
  tell [Assignment (TAC joined (Join (fromIntegral joinSize)
                                renamedLHS renamedRHS))]

  let args = (Relation joined (Right <$> joinedParams), NotNegated)
             : deleteAt lhs (deleteAt rhs subgoals)

  projectUnused =<< removeUnused (Rule relH args)

  where
    variableUsedIn :: Map Name (Set SubgoalIndex)
    variableUsedIn =
      Map.fromListWith Set.union
      $ concatMap
        (\(ix, (Relation _ vars, _)) -> map (, Set.singleton ix)
                                        (filter (not . isNameUnused) (rights vars)))
      $ zip [0..] subgoals

    joinPairs :: Map (SubgoalIndex, SubgoalIndex) (Set Name)
    joinPairs =
      Map.fromListWith Set.union
      $ concatMap (\(var, ps) -> (, Set.singleton var) <$> Set.toList ps)
      $ Map.toList (pairsOf <$> variableUsedIn)

    lhs, rhs :: SubgoalIndex
    joinVars :: Set Name
    ((lhs, rhs), joinVars) = Extra.maximumOn (Set.size . snd)
                             $ Map.toList joinPairs

    Relation lhsRel lhsVars = fst (subgoals !! lhs)
    Relation rhsRel rhsVars = fst (subgoals !! rhs)

    joinSize = Set.size joinVars

    permutationLHS, permutationRHS :: AttrPermutation
    permutationLHS =
      let names = rights lhsVars
          joinIndices = map (`unsafeElemIndex` names)
                        $ toList joinVars
          notJoinIndices = map fst
                           $ filter (not . (`Set.member` joinVars) . snd)
                           $ zip [0..] names
      in notJoinIndices ++ joinIndices
    permutationRHS =
      let names = rights rhsVars
          joinIndices = map (`unsafeElemIndex` names)
                        $ toList joinVars
          notJoinIndices = map fst
                           $ filter (not . (`Set.member` joinVars) . snd)
                           $ zip [0..] names
      in joinIndices ++ notJoinIndices

    unsafeElemIndex :: Eq a => a -> [a] -> Int
    unsafeElemIndex el xs = fromMaybe (error "unsafeElemIndex: out of bounds") (List.elemIndex el xs)

    joinedParams :: [Name]
    joinedParams =
      applyPermutation permutationLHS (rights lhsVars)
      ++ drop joinSize (applyPermutation permutationRHS (rights rhsVars))

--------------------------------------------------------------------------------

generateEverythings
  :: (MonadTAC rel m)
  => Declaration rel Name
  -> WriterT [Statement rel] m (Declaration rel Name)
generateEverythings (Rule (Relation relH argsH) exprs) = do
  rel <- freshRel
  let lhsVars :: Set Name
      lhsVars = Set.fromList $ concatMap toList argsH
      rhsVars :: Set Name
      rhsVars = Set.fromList $ concatMap (toList . fst) exprs
      args :: [Either a Name]
      args = map Right $ Set.toList $ lhsVars `Set.difference` rhsVars
  tell [Assignment (TAC rel Everything)]
  pure (Rule (Relation relH argsH) ((Relation rel args, NotNegated) : exprs))

--------------------------------------------------------------------------------

joinEverythingElse
  :: (MonadTAC rel m)
  => Declaration rel Name
  -> WriterT [Statement rel] m (Declaration rel Name)
joinEverythingElse (Rule relH [])     = pure (Rule relH [])
joinEverythingElse (Rule relH [expr]) = pure (Rule relH [expr])
joinEverythingElse (Rule relH (exprA : exprB : rest)) = do
  let (Relation relA argsA, _) = exprA
  let (Relation relB argsB, _) = exprB
  joined <- freshRel
  tell [Assignment (TAC joined (Join 0 relA relB))]
  joinEverythingElse
    $ Rule relH ((Relation joined (argsA ++ argsB), NotNegated) : rest)

--------------------------------------------------------------------------------

-- might be 4 and 5 in thesis.
renameToHead
  :: (MonadTAC rel m)
  => Declaration rel Name
  -> WriterT [Statement rel] m (Declaration rel Name)
renameToHead (Rule relH []) = pure (Rule relH [])
renameToHead (Rule (Relation relH argsH) [(Relation rel args, NotNegated)]) = do
  -- project away things in args but not in argsH
  --
  -- if there are any constants in relH, use `Const` to generate new columns and
  -- then join them on with size 0
  --
  let thingsInArgsHButNotInArgs :: [Attr]
      thingsInArgsHButNotInArgs = map
        (fromMaybe (error "renameToHead: difference of opinions")
          . (`List.elemIndex` args)
        )
        $ filter isRight argsH

  let constantsInArgsH :: [Constant]
      constantsInArgsH = lefts argsH

  projectedRel <- freshRel
  tell [Assignment (TAC projectedRel (Project thingsInArgsHButNotInArgs rel))]

  constRel <- freshRel
  tell [Assignment (TAC constRel (Const constantsInArgsH))]

  joinRel <- freshRel
  tell [Assignment (TAC joinRel (Join 0 projectedRel constRel))]

  -- needs to be a permutation of argsH
  let args' :: [Either Constant Name]
      args' = map (args !!) (List.sort thingsInArgsHButNotInArgs) ++ map Left constantsInArgsH

  let perm = fromMaybe (error "renameToHead: computePermutation did not succeed") (computePermutation args' argsH)
  renameRel <- freshRel
  tell [Assignment (TAC renameRel (Rename perm joinRel))]

  pure (Rule (Relation relH argsH) [(Relation renameRel argsH, NotNegated)])
renameToHead _ = error "renameToHead: precondition violation"

--------------------------------------------------------------------------------

canoniseConstants
  :: (MonadTAC rel m)
  => Declaration rel Name
  -> WriterT [Statement rel] m (Declaration rel Name)
canoniseConstants (Rule relH exprs) = do
  let fixConstant :: Constant -> Constant
      fixConstant = \case
        ConstantBitString b -> ConstantBitString b
        ConstantInt i -> ConstantBitString (map (testBit i) [0..finiteBitSize i - 1])
        ConstantBool b -> ConstantBitString [b]
      fixRelation :: Relation rel Name -> Relation rel Name
      fixRelation (Relation rel args) = Relation rel (map (either (Left . fixConstant) Right) args)
  pure (Rule (fixRelation relH) (map (first fixRelation) exprs))

--------------------------------------------------------------------------------

unifyHeadRelation
  :: (MonadTAC rel m)
  => Declaration rel Name
  -> WriterT [Statement rel] m ()
-- we don't seem to run into this in practise?
--unifyHeadRelation (Rule rel []) = do
--  error $ pretty rel
unifyHeadRelation (Rule (Relation relH argsH) [(Relation rel args, NotNegated)]) = do
  unless (args == argsH) (error "unifyHeadRelation: precondition violation")
  tell [Assignment (TAC relH (Union relH rel))]
unifyHeadRelation _ = error "unifyHeadRelation: precondition violation"

--------------------------------------------------------------------------------

isNameUnused :: Name -> Bool
isNameUnused = \case
  ParseName       m -> isNothing m
  ElaborationName m -> isNothing m

--------------------------------------------------------------------------------

-- | Compute the permutation from one list to another, if one exists.
--
-- Laws:
-- 1. If @Just p = computePermutation xs ys@, then @ys = applyPermutation p xs@.
computePermutation :: Eq a => [a] -> [a] -> Maybe AttrPermutation
computePermutation xs ys = mapM (\x -> List.elemIndex x ys) xs

applyPermutation :: AttrPermutation -> [a] -> [a]
applyPermutation perm xs = map (xs !!) perm

--------------------------------------------------------------------------------

temparts :: forall s a. (Data s, Typeable a) => Lens' s [a]
temparts = unsafePartsOf template

flipEither :: Either a b -> Either b a
flipEither = either Right Left

hasConstant :: Relation rel var -> Bool
hasConstant = any isLeft . relArguments

needsJoin :: [Expr rel Name] -> Bool
needsJoin = any (> 1)
            . Map.fromListWith (+)
            . map (, 1 :: Int)
            . rights
            . concatMap (relArguments . fst)

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

setNub :: Ord a => [a] -> [a]
setNub = Set.toList . Set.fromList

--------------------------------------------------------------------------------
