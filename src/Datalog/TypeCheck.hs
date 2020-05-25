{-# LANGUAGE ScopedTypeVariables #-}

module Datalog.TypeCheck where

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

