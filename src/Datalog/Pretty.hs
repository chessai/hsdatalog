{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}

module Datalog.Pretty where

import Data.Bool (bool)
import Data.Int (Int8)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Text (Text)
import Datalog.Graph
import Datalog.RelAlgebra
import Datalog.Syntax
import Datalog.TypeCheck

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

class Pretty a where
  pretty :: a -> String

instance Pretty Int where
  pretty = show

instance Pretty Int8 where
  pretty = show

instance Pretty Bool where
  pretty = bool "#false" "#true"

instance Pretty Text where
  pretty = T.unpack

instance Pretty a => Pretty [a] where
  pretty = ('[' :) . (++ "]") . intercalate ", " . map pretty

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty = either pretty pretty

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (x, y) = "(" ++ pretty x ++ ", " ++ pretty y ++ ")"

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

instance Pretty Type where
  pretty = \case
    TypeInt -> "Int"
    TypeBool -> "Bool"
    TypeBitString n -> "BitString " ++ pretty n
    TypeVar _ -> "<inference type>"
    TypeRelation tys -> "Relation(" ++ intercalate ", " (map pretty tys) ++ ")"

instance Pretty Name where
  pretty = \case
    ParseName m -> fromMaybe "_" m
    ElaborationName m -> maybe "_" (("elab" ++) . pretty) m

instance (Pretty rel) => Pretty (RelAlgebra rel) where
  pretty = \case
    Not rel -> "¬" ++ pretty rel
    Const cs -> pretty cs
    -- was "⋈" but you can barely see it
    Join sub x y -> pretty x ++ " ∞" ++ showSubscript sub ++ " " ++ pretty y
    Union x y -> pretty x ++ " ∪ " ++ pretty y
    Project attrs rel -> "π(" ++ pretty attrs ++ ", " ++ pretty rel ++ ")"
    Rename perm rel -> "ρ(" ++ pretty perm ++ ", " ++ pretty rel ++ ")"
    Difference x y -> pretty x ++ " – " ++ pretty y
    Select attr cnst rel -> "σ(" ++ pretty attr ++ " = " ++ pretty cnst ++ ", " ++ pretty rel ++ ")"
    Everything -> "everything"

instance (Pretty rel) => Pretty (Statement rel) where
  pretty = \case
    While rel s -> "while " ++ pretty rel ++ " " ++ pretty s ++ ";"
    Block ss -> "{\n" ++ indent 4 (concatMap ((++ ";\n") . pretty) ss) ++ "}"
    Assignment t -> pretty t

instance (Pretty rel) => Pretty (TAC rel) where
  pretty (TAC rel alg) = pretty rel ++ " := " ++ pretty alg

instance Pretty Rel where
  pretty = \case
    EqualityConstraint -> "~"
    ElaborationRel i -> show i
    ParseRel s -> s

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty = pretty . Map.toList

prettyExpr :: (Pretty rel, Pretty var) => Expr rel var -> String
prettyExpr (rel, negated) = bool "!" "" (isNotNegated negated) ++ pretty rel

prettyType :: (Pretty name) => name -> Type -> String
prettyType name typ = pretty name ++ " : " ++ pretty typ ++ "."

prettyGraph :: (Pretty node, Pretty weight) => Graph node weight -> String
prettyGraph = unlines . map go . edges
  where
    go (source, target, weight) = pretty source ++ " -> " ++ pretty target ++ "; [" ++ pretty weight ++ "]"

showSubscript :: (Integral a, Show a) => a -> String
showSubscript = map toSubscript . show
  where
    toSubscript :: Char -> Char
    toSubscript = \case
      '0'   -> '₀'
      '1'   -> '₁'
      '2'   -> '₂'
      '3'   -> '₃'
      '4'   -> '₄'
      '5'   -> '₅'
      '6'   -> '₆'
      '7'   -> '₇'
      '8'   -> '₈'
      '9'   -> '₉'
      other -> other

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines
