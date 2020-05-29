{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Datalog.RelAlgebra where

import Control.Monad
import Control.Monad.State.Class (get, modify, put)
import Control.Monad.State.Strict (State)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Datalog.Syntax
import Numeric.Natural

import qualified Data.Map.Strict as Map

type Attr = Int

type AttrPermutation = [Attr]

data RelAlgebra rel
  = Not rel
  | Join Natural rel rel
  | Union rel rel
  | Project [Attr] rel
  | Rename AttrPermutation rel
  | Difference rel rel
  | Select Attr Constant rel -- Int constants are all we have right now
  deriving stock (Eq, Ord, Show)
  deriving stock (Functor, Foldable, Traversable)

{-
interpret :: forall m rel attr. (Monad m, Ord rel) => [(rel, RelAlgebra attr rel)] -> Map rel DDNode -> CuddT m (Map rel DDNode)
interpret tacs m = foldlM (flip (uncurry go)) m tacs
  where
    go :: rel -> RelAlgebra attr rel -> Map rel DDNode -> CuddT m (Map rel DDNode)
    go rel alg m = do
      node <- go' (looksey m <$> alg)
      pure (Map.insert rel node m)

    looksey :: Map rel rel' -> rel -> rel'
    looksey m rel = Map.findWithDefault (error "looksey fail") rel m

    go' :: RelAlgebra attr DDNode -> CuddT m DDNode
    go' = \case
      Join a b -> undefined
      Union a b -> undefined
      Project attrs rel -> do

        undefined
      Rename attrA attrB rel -> undefined
      Difference a b -> Cudd.and a =<< Cudd.not b
      Select attr c rel -> undefined
-}

data TAC rel
  = TAC rel (RelAlgebra rel)
  deriving stock (Eq, Ord, Show)

data Statement rel
  = While rel (Statement rel)
  | Block [Statement rel]
  | Assignment (TAC rel)
  deriving stock (Eq, Ord, Show)

newtype Tuple = Tuple [Constant]  deriving (Eq, Ord, Show)
newtype Table = Table (Set Tuple) deriving (Eq, Ord, Show)

emptyTable :: Table
emptyTable = Table mempty

referenceInterpreter
  :: forall rel
  .  (Ord rel)
  => Statement rel
  -> State (Map rel Table) ()
referenceInterpreter (While rel stmt) = do
  before <- Map.findWithDefault emptyTable rel <$> get
  referenceInterpreter stmt
  after <- Map.findWithDefault emptyTable rel <$> get
  when (before /= after) $ referenceInterpreter (While rel stmt)
referenceInterpreter (Block stmts) = mapM_ referenceInterpreter stmts
referenceInterpreter (Assignment (TAC lhs rhs)) = do
  state <- get
  let go :: RelAlgebra rel -> Table
      go = undefined
  modify (Map.insert lhs (go rhs))
