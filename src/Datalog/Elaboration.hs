{-# language ScopedTypeVariables #-}
{-# language TupleSections #-}

module Datalog.Elaboration where

import Control.Monad.State.Class (get, put, modify)
import Control.Monad.State.Strict (State, evalState)
import Data.Bifunctor (Bifunctor, bimap, first)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Datalog.Syntax
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type Name = Maybe Int

used :: (Foldable t, Ord var) => t var -> ([var], [var])
used d =
  let allVars = Map.toAscList (Map.fromListWith (+) (zip (toList d) (repeat 1)))
  in mapBoth (map fst) (List.partition ((== 1) . snd) allVars)

renameProgram :: forall rel var. (Ord var) => Program rel var -> Program rel Name
renameProgram = normaliseRules . removeSubgoalDuplication

normaliseRules :: forall rel var. (Ord var) => Program rel var -> Program rel Name
normaliseRules = flip evalState 0 . traverse go
  where
    go :: Declaration rel var -> State Int (Declaration rel Name)
    go d = do
      let (usedOnce, usedMany) = used d
      n <- get
      let renamings = Map.fromList (map (,Nothing) usedOnce ++ zip usedMany (Just <$> [n..]))
      modify (+ length usedMany)
      pure (fmap (renamings Map.!) d)

-- FIXME: make this do its damn JOB!
removeSubgoalDuplication :: forall rel var. (Ord var) => Program rel var -> Program rel (var, Int)
removeSubgoalDuplication = map (fmap (,0))
{-
  map go
  where
    go :: Declaration rel var -> Declaration rel (var, Int)
    go (Rule relH exprs) = Rule ((,0) <$> relH) (concat (evalState (traverse foo exprs) 1))
      --let (usedOnce, usedMany) = used exprs
      --in undefined

    foo :: Expr rel var -> State Int [Expr rel (var, Int)]
    foo (Relation rel vars, b) = do
      let (usedOnce, usedMany) = used vars
      n <- get
      undefined
-}

mapBoth :: Bifunctor f => (a -> b) -> f a a -> f b b
mapBoth f = bimap f f


