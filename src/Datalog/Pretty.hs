{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}

module Datalog.Pretty where

import           Data.Bool (bool)
import           Data.List (intercalate)
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Datalog.Syntax
import           Datalog.TypeCheck

class Pretty a where
  pretty :: a -> String

instance Pretty Int where
  pretty = show

instance Pretty Bool where
  pretty = bool "#false" "#true"

instance Pretty Text where
  pretty = T.unpack

instance Pretty a => Pretty [a] where
  pretty = ('[' :) . (++ "]") . intercalate ", " . map pretty

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

prettyExpr :: (Pretty rel, Pretty var) => Expr rel var -> String
prettyExpr (rel, negated) = bool "!" "" (isNotNegated negated) ++ pretty rel

prettyType :: (Pretty name) => name -> Type -> String
prettyType name typ = pretty name ++ " : " ++ pretty typ ++ "."
