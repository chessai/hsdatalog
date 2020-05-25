{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DerivingStrategies  #-}
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

  , parseProgram
  , negatedToBool
  ) where

import Control.Monad
import Control.Monad.State.Class (get, modify, put)
import Control.Monad.State.Strict (State, evalState, execState)
import Data.Bifunctor (Bifunctor, bimap, first)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------

data Constant
  = ConstantInt Int
  | ConstantBool Bool
  | ConstantBitString [Bool]
  deriving stock (Eq, Show)

data Relation rel var = Relation
  { relRelation :: rel
  , relArguments :: [Either Constant var]
  }
  deriving stock (Eq, Show)
  deriving stock (Functor, Foldable)

data Negated = Negated | NotNegated
  deriving stock (Eq, Show)

negatedToBool :: Negated -> Bool
negatedToBool Negated = False
negatedToBool NotNegated = True

type Expr rel var = (Relation rel var, Negated)

data Declaration rel var
  = Rule (Relation rel var) [Expr rel var]
    -- ^ rulename_lhs(var1, var2) :- rulename_rhs1(vars...), rulename_rhs2(vars...), ..., rulename_rhsN(vars...).
  deriving stock (Eq, Show)
  deriving stock (Functor, Foldable)

data Program rel var = Program
  { decls :: [Declaration rel var]
  , types :: Map rel Type
  }
  deriving stock (Eq, Show)
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
  deriving stock (Eq, Show)

--------------------------------------------------------

type Parser = ParsecT Void String (State (Map String Type))

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
identifier = (lexeme . try) (ident >>= check)
  where
    ident = (:) <$> letterChar <*> many alphaNumChar
    check = pure

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

program :: Parser (Program String String)
program = do
  decls <- concat <$> between sc eof (many topLevel)
  types <- get
  pure (Program decls types)

-- TODO: think about the following situation:
--
-- if we extend the syntax such that the typing state can be updated, but then a
-- failure occurs and we have to backtrack, is the state change undone?
topLevel :: Parser [Declaration String String]
topLevel = try (typeSignature *> pure []) <|> ((:[]) <$> declaration)

declaration :: Parser (Declaration String String)
declaration = do
  rel <- relation
  rels <- try (infersFrom *> exprs) <|> (period *> pure [])
  pure (Rule rel rels)

relation :: Parser (Relation String String)
relation = do
  name <- identifier
  vars <- parens (rhs `sepBy` comma)
  pure (Relation name vars)
  where
    rhs = (Left <$> constant) <|> (Right <$> identifier)

typeSignature :: Parser ()
typeSignature = do
  name <- identifier
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

expr :: Parser (Relation String String, Negated)
expr = do
  negated <- maybe NotNegated (const Negated) <$> try (optional (symbol "!"))
  rel <- relation
  pure (rel, negated)

exprs :: Parser [(Relation String String, Negated)]
exprs = between sc period (expr `sepBy` comma)

parseProgram :: FilePath -> String -> Either String (Program String String)
parseProgram file input = first errorBundlePretty $ flip evalState Map.empty $ runParserT program file input
