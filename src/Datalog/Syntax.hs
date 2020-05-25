{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Datalog.Syntax
  ( Relation(..)
  , Expr
  , Declaration(..)
  , Program
  , Constant

  , parseProgram
  ) where

import           Control.Monad
import           Control.Monad.State.Class  (get, modify, put)
import           Control.Monad.State.Strict (State, evalState)
import           Data.Bifunctor             (Bifunctor, bimap, first)
import           Data.Foldable              (toList)
import qualified Data.List                  as List
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------

type Constant = Int

data Relation rel var = Relation rel [Either Constant var]
  deriving stock (Eq, Show)
  deriving stock (Functor, Foldable)

type Expr rel var = (Relation rel var, Bool)

data Declaration rel var
  = Rule (Relation rel var) [Expr rel var]
    -- ^ rulename_lhs(var1, var2) :- rulename_rhs1(vars...), rulename_rhs2(vars...), ..., rulename_rhsN(vars...).
  deriving stock (Eq, Show)
  deriving stock (Functor, Foldable)

type Program rel var = [Declaration rel var]

--------------------------------------------------------

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space (void spaceChar) lineCmt blockCmt
  where
    lineCmt = L.skipLineComment "//"
    blockCmt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

comma :: Parser ()
comma = void (symbol ",")

leftArrow :: Parser ()
leftArrow = void (symbol "<-")

period :: Parser ()
period = void (symbol ".")

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

reserved :: String -> Parser ()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

identifier :: Parser String
identifier = (lexeme . try) (ident >>= check)
  where
    ident = (:) <$> letterChar <*> many alphaNumChar
    check = pure

constant :: Parser Int
constant = (lexeme . try) L.decimal

program :: Parser (Program String String)
program = between sc eof (many declaration)

declaration :: Parser (Declaration String String)
declaration = do
  rel <- relation
  rels <- do
    try (leftArrow *> exprs)
    <|> (period *> pure [])
  pure (Rule rel rels)

relation :: Parser (Relation String String)
relation = do
  name <- identifier
  vars <- parens (rhs `sepBy` comma)
  pure (Relation name vars)
  where
    rhs = (Left <$> constant) <|> (Right <$> identifier)

expr :: Parser (Relation String String, Bool)
expr = do
  negated <- isJust <$> try (optional (symbol "!"))
  rel <- relation
  pure (rel, not negated)

exprs :: Parser [(Relation String String, Bool)]
exprs = between sc period (expr `sepBy` comma)

parseProgram :: FilePath -> String -> Either String (Program String String)
parseProgram file input = first errorBundlePretty $ runParser program file input
