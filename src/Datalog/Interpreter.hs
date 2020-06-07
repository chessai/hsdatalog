{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

module Datalog.Interpreter where

import Control.Monad
import Control.Monad.State.Class (get, modify)
import Control.Monad.State.Strict (State, StateT, execStateT)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Datalog.Cudd (Cudd, DDNode, SatBit)
import Datalog.Elaboration
import Datalog.RelAlgebra
import Datalog.Syntax

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Datalog.Cudd as Cudd

everythingIsTyped :: (Ord rel) => RelProgram rel -> Bool
everythingIsTyped (RelProgram statement typs)
  = Set.fromList (toList statement) `Set.isSubsetOf` Map.keysSet typs

assert :: Applicative m => String -> Bool -> m ()
assert s b = if b then pure () else error ("ASSERTION FAILED: " ++ s)

type Interpret rel = StateT (Map rel DDNode) Cudd

interpretProgram :: (Ord rel) => RelProgram rel -> Map rel [[SatBit]]
interpretProgram = runInterpret . interpret

runInterpret :: (Ord rel) => Interpret rel void -> Map rel [[SatBit]]
runInterpret i = Cudd.chew $ do
  m <- execStateT i Map.empty
  traverse Cudd.allSat m

interpret :: forall rel. (Ord rel) => RelProgram rel -> Interpret rel ()
interpret relProgram@(RelProgram statement typs) = do
  assert "everything in the program is typed" (everythingIsTyped relProgram)

  alwaysFalse <- lift Cudd.readLogicZero
  alwaysTrue <- lift Cudd.readOne
  let go :: Statement rel -> Interpret rel ()
      go = \case
        While rels stmt -> do
          before <- (\m -> map (flip (Map.findWithDefault alwaysFalse) m) rels) <$> get
          go stmt
          after <- (\m -> map (flip (Map.findWithDefault alwaysFalse) m) rels) <$> get
          when (before /= after) $ go (While rels stmt)
        Block block -> do
          mapM_ go block
        Assignment (TAC lhs rhs) -> do
          m <- get
          let lookupNode rel = Map.findWithDefault alwaysFalse rel m
          let relToDDNode :: RelAlgebra rel -> Cudd DDNode
              relToDDNode = \case
                Not rel -> do
                  Cudd.not (lookupNode rel)
                Const constants -> do
                  let constant = concatMap (\(ConstantBitString x) -> x) constants
                  vars <- mapM Cudd.ithVar [0..length constant - 1]
                  nottedVars <- zipWithM (\var c -> if c then pure var else Cudd.not var)
                                         vars
                                         constant
                  foldM Cudd.and alwaysTrue nottedVars
                Join (fromIntegral -> n) x y -> do
                  let TypeRelation reltypsx = typs Map.! x
                  let TypeRelation reltypsy = typs Map.! y
                  let shiftY = sum $ map typeBitWidth $ take (length reltypsx - n) reltypsx
                  let ybits = sum $ map typeBitWidth reltypsy
                  Cudd.and (lookupNode x) =<< Cudd.permute (lookupNode y) [shiftY .. shiftY + ybits]
                Union x y -> do
                  Cudd.or (lookupNode x) (lookupNode y)
                Project attrs rel -> do
                  let TypeRelation reltyps = typs Map.! rel
                  let deleted :: [Attr]
                      deleted =
                        Set.toList
                        $ Set.difference (Set.fromList [0..length reltyps - 1]) (Set.fromList attrs)
                  let bitWidths = map typeBitWidth reltyps
                  let accumSumsBitWidths = accumSums bitWidths
                  let deletedIndices = concatMap
                        (\i ->
                          let a = accumSumsBitWidths !! i
                              b = bitWidths !! i
                          in [a .. a + b - 1]
                        )
                        deleted
                  vars <- mapM Cudd.ithVar deletedIndices
                  foldM Cudd.exists (lookupNode rel) vars
                Rename perm rel -> do
                  let TypeRelation reltyps = typs Map.! rel
                  let bitWidths = map typeBitWidth reltyps
                  let accumSumsBitWidths = accumSums bitWidths
                  let indices = map
                            (\i ->
                              let a = accumSumsBitWidths !! i
                                  b = bitWidths !! i
                              in [a .. a + b - 1]
                            )
                            [0..length reltyps - 1]
                  let bitPerm = concat $ applyPermutation perm indices
                  Cudd.permute (lookupNode rel) bitPerm
                Difference x y -> do
                  Cudd.and (lookupNode x) =<< Cudd.not (lookupNode y)
                Select attr (ConstantBitString bs) rel -> do
                  let TypeRelation reltyps = typs Map.! rel
                  let offset = sum $ map typeBitWidth $ take (attr - 1) reltyps
                  Cudd.restrict' (lookupNode rel) (Map.fromList (zip [offset..] bs))
                Everything -> do
                  pure alwaysTrue
          ddnode <- lift $ relToDDNode rhs
          modify (Map.insert lhs ddnode)

  go statement

accumSums :: [Int] -> [Int]
accumSums = scanl (+) 0

newtype Tuple = Tuple [Constant]
  deriving newtype (Eq, Ord)
  deriving stock (Show)
newtype Table = Table (Set Tuple)
  deriving newtype (Eq, Ord)
  deriving stock (Show)

emptyTable :: Table
emptyTable = Table mempty

referenceInterpreter
  :: forall rel
  .  (Ord rel)
  => Statement rel
  -> State (Map rel Table) ()
referenceInterpreter (While rels stmt) = do
  before <- (\m -> map (flip (Map.findWithDefault emptyTable) m) rels) <$> get
  referenceInterpreter stmt
  after <- (\m -> map (flip (Map.findWithDefault emptyTable) m) rels) <$> get
  when (before /= after) $ referenceInterpreter (While rels stmt)
referenceInterpreter (Block stmts) = mapM_ referenceInterpreter stmts
referenceInterpreter (Assignment (TAC lhs rhs)) = do
  state <- get
  let go :: RelAlgebra rel -> Table
      go = undefined
  modify (Map.insert lhs (go rhs))
