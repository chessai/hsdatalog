{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Datalog.TypeCheck where

import Control.Monad.State.Class (get, put)
import Control.Monad.State.Strict (State, execState)
import Data.Bifunctor (second)
import Data.Either (rights)
import Data.List.Index (iforM, itraverse)
import Data.Map.Strict (Map)
import Data.Set (Set)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Datalog.Syntax

type TypeEquality = (Type, Type)

typeEnvironment :: forall rel var. Program rel var -> Map rel Type
typeEnvironment p = undefined
--  where
--    _ :: Declaration rel var -> (Map rel Type, Set TypeEquality)

data Position
  = Lhs Int -- index into lhs variables
  | Rhs Int Int -- (index into subgoal, index into subgoal variables)
  deriving (Eq, Ord, Show)

positionTypes :: forall rel var. (Ord rel, Ord var) => Map rel Type -> Declaration rel var -> State Int (Map Position Type)
positionTypes userWrittenTypes (Rule relH exprs) = do
  m <- processRelation Lhs relH
  ms <- itraverse (\i -> processRelation (Rhs i) . fst) exprs
  pure (Map.unions (m : ms))
  where
    processRelation :: (Int -> Position) -> Relation rel var -> State Int (Map Position Type)
    processRelation toPosition (Relation rel vars) = do
      case Map.lookup rel userWrittenTypes of
        Nothing -> do
          fmap Map.fromList $ iforM vars $ \i -> \case
            Left c -> pure (toPosition i, constantToType c)
            Right var -> undefined

          --tyVars <- replicateM (length vars) _
          --imapM (\i var ->
        Just typ -> do
          undefined

constantToType :: Constant -> Type
constantToType = \case
  ConstantInt _ -> TypeInt
  ConstantBool _ -> TypeBool
  ConstantBitString s -> TypeBitString (length s)

variablePositions :: forall rel var. (Ord var) => Declaration rel var -> Map var (Set Position)
variablePositions (Rule (Relation _ vars) exprs) = Map.unionWith Set.union lhsPositions rhsPositions
  where
    lhsPositions, rhsPositions :: Map var (Set Position)
    lhsPositions =
      Map.fromListWith Set.union
      $ map (\(ix, var) -> (var, Set.singleton (Lhs ix)))
      $ rights
      $ map sequenceA
      $ zip [0..] vars

    rhsPositions =
      Map.fromListWith Set.union
      $ map (\(subgoalIx, (argIx, var)) -> (var, Set.singleton (Rhs subgoalIx argIx)))
      $ concatMap sequenceA
      $ map (second (rights . map sequenceA . zip [0..] . relArguments))
      $ zip [0..] (map fst exprs)
