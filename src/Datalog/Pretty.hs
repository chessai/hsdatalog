{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}

module Datalog.Pretty where

import Data.Bool (bool)
import Data.List (intercalate)
import Datalog.Syntax

class Pretty a where
  pretty :: a -> String

instance Pretty Int where
  pretty = show

instance Pretty Bool where
  pretty = bool "#false" "#true"

instance (a ~ Char) => Pretty [a] where
  pretty = id

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty = either pretty pretty

instance Pretty Constant where
  pretty = \case
    ConstantInt i -> pretty i
    ConstantBool b -> pretty b
    ConstantBitString bs -> '#' : map (bool '0' '1') bs

instance (Pretty rel, Pretty var) => Pretty (Relation rel var) where
  pretty (Relation rel args) = pretty rel ++ "(" ++ intercalate ", " (map pretty args) ++ ")"

instance (Pretty rel, Pretty var) => Pretty (Declaration rel var) where
  pretty (Rule rel []) = pretty rel ++ "."
  pretty (Rule rel exprs) = pretty rel ++ " :- " ++ intercalate ", " (map prettyExpr exprs) ++ "."

prettyExpr :: (Pretty rel, Pretty var) => Expr rel var -> String
prettyExpr (rel, negated) = bool "!" "" (negatedToBool negated) ++ pretty rel

