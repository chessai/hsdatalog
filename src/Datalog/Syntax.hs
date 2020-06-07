{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Datalog.Syntax
  ( Relation(..)
  , Negated(..)
  , Expr
  , Declaration(..)
  , Program(..)
  , Constant(..)
  , Type(..)
  , Var
  , Name(..)
  , Rel(..)

  , parseProgram
  , isNotNegated
  ) where

import Control.Monad
import Control.Monad.State.Class (get, modify, put)
import Control.Monad.State.Strict (State, evalState, execState)
import Data.Bifunctor (Bifunctor, bimap, first)
import Data.Data (Data)
import Data.Foldable (toList)
import Data.Int (Int8)
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Void (Void)
import GHC.Generics (Generic)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------

type Var = Int

data Name
  = ParseName (Maybe String)
  | ElaborationName (Maybe Var)
  deriving stock (Eq, Ord, Show, Generic, Data)

data Rel = EqualityConstraint Type | ElaborationRel Int | ParseRel String
  deriving stock (Eq, Ord, Show, Data)

data Constant
  = ConstantInt Int8 -- FIXME: change back to Int, only Int8 for ease of reading while developing
  | ConstantBool Bool
  | ConstantBitString [Bool]
  deriving stock (Eq, Ord, Show, Generic, Data)

data Relation rel var = Relation
  { relRelation :: rel
  , relArguments :: [Either Constant var]
  }
  deriving stock (Eq, Show, Generic)
  deriving stock (Functor, Foldable, Data)

data Negated = Negated | NotNegated
  deriving stock (Eq, Ord, Show, Generic, Data)

isNotNegated :: Negated -> Bool
isNotNegated Negated = False
isNotNegated NotNegated = True

type Expr rel var = (Relation rel var, Negated)

data Declaration rel var
  = Rule (Relation rel var) [Expr rel var]
    -- ^ rulename_lhs(var1, var2) :- rulename_rhs1(vars...), rulename_rhs2(vars...), ..., rulename_rhsN(vars...).
  deriving stock (Eq, Show, Generic, Data)
  deriving stock (Functor, Foldable)

data Program rel var = Program
  { decls :: [Declaration rel var]
  , types :: Map rel Type
  }
  deriving stock (Eq, Show, Generic, Data)
  deriving stock (Functor, Foldable)

-- TODO: expand this
data Type
  = TypeInt
    -- ^ machine integer
  | TypeBool
    -- ^ boolean
  | TypeBitString Int
    -- ^ bit strings of length n
  | TypeVar Int
    -- ^ used during type inference
  | TypeRelation [Type]
    -- ^ top level type signature
  deriving stock (Eq, Ord, Show, Generic, Data)

--------------------------------------------------------

type Parser = ParsecT Void String (State (Map Rel Type))

sc :: Parser ()
sc = L.space (void spaceChar) lineCmt blockCmt
  where
    lineCmt = L.skipLineComment "//"
    blockCmt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

comma :: Parser ()
comma = void (symbol ",")

infersFrom :: Parser ()
infersFrom = void (symbol ":-")

period :: Parser ()
period = void (symbol ".")

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

reserved :: String -> Parser ()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

identifier :: Parser String
identifier = (lexeme . try) ident
  where
    ident = (:) <$> letterChar <*> many alphaNumChar

name :: Parser Name
name = fmap (ParseName . Just) identifier

rel :: Parser Rel
rel = ParseRel <$> identifier

bool :: Parser Bool
bool = (True <$ reserved "#true") <|> (False <$ reserved "#false")

bitString :: Parser [Bool]
bitString = do
  symbol "#"
  many ((True <$ symbol "1") <|> (False <$ symbol "0"))

constant :: Parser Constant
constant = (lexeme . try)
  ( (ConstantInt <$> L.decimal)
    <|> (ConstantBool <$> bool)
    <|> (ConstantBitString <$> bitString)
  )

program :: Parser (Program Rel Name)
program = do
  decls <- concat <$> between sc eof (many topLevel)
  types <- get
  pure (Program decls types)

-- TODO: think about the following situation:
--
-- if we extend the syntax such that the typing state can be updated, but then a
-- failure occurs and we have to backtrack, is the state change undone?
topLevel :: Parser [Declaration Rel Name]
topLevel = try (typeSignature *> pure []) <|> ((:[]) <$> declaration)

declaration :: Parser (Declaration Rel Name)
declaration = do
  rel <- relation
  rels <- try (infersFrom *> exprs) <|> (period *> pure [])
  pure (Rule rel rels)

relation :: Parser (Relation Rel Name)
relation = do
  name <- rel
  vars <- parens (rhs `sepBy` comma)
  pure (Relation name vars)
  where
    rhs = (Left <$> constant) <|> (Right <$> name)

typeSignature :: Parser ()
typeSignature = do
  name <- rel
  symbol ":"
  reserved "Relation"
  types <- parens (typ `sepBy`comma)
  period
  modify (Map.insert name (TypeRelation types))

typ :: Parser Type
typ = do
      (TypeInt <$ reserved "Int")
  <|> (TypeBool <$ reserved "Bool")
  <|> (reserved "BitString" *> (TypeBitString <$> L.decimal))

expr :: Parser (Relation Rel Name, Negated)
expr = do
  negated <- maybe NotNegated (const Negated) <$> try (optional (symbol "!"))
  rel <- relation
  pure (rel, negated)

exprs :: Parser [(Relation Rel Name, Negated)]
exprs = between sc period (expr `sepBy` comma)

parseProgram :: FilePath -> String -> Either String (Program Rel Name)
parseProgram file input = first errorBundlePretty $ flip evalState Map.empty $ runParserT program file input
