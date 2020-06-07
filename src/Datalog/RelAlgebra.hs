{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Datalog.RelAlgebra where

import Control.Monad
import Control.Monad.State.Class (get, modify, put)
import Control.Monad.State.Strict (State)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Datalog.Syntax
import GHC.Generics (Generic)
import Numeric.Natural

import qualified Data.Map.Strict as Map

type Attr = Int

type AttrPermutation = [Attr]

data RelAlgebra rel
  = Not rel
  | Const [Constant]
  | Join Natural rel rel
  | Union rel rel
  | Project [Attr] rel -- TODO: should be (Set Attr)
  | Rename AttrPermutation rel
  | Difference rel rel
  | Select Attr Constant rel -- Int constants are all we have right now
  | Everything -- the relation that contains everything
  deriving stock (Eq, Ord, Show, Generic)
  deriving stock (Functor, Foldable, Traversable)

data RelProgram rel = RelProgram
  { relProgramStatement :: Statement rel
  , relProgramTypes :: Map rel Type
  }
  deriving stock (Eq, Ord, Show, Generic)

data TAC rel
  = TAC rel (RelAlgebra rel)
  deriving stock (Eq, Ord, Show, Generic)
  deriving stock (Functor, Foldable, Traversable)

data Statement rel
  = While [rel] (Statement rel)
  | Block [Statement rel]
  | Assignment (TAC rel)
  deriving stock (Eq, Ord, Show, Generic)
  deriving stock (Functor, Foldable, Traversable)

statementToTACs :: Statement rel -> [TAC rel]
statementToTACs = \case
  While _ s -> statementToTACs s
  Block block -> concatMap statementToTACs block
  Assignment tac -> [tac]

