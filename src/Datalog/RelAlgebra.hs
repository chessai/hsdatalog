{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DerivingStrategies #-}

module Datalog.RelAlgebra where

import Datalog.Syntax
import Numeric.Natural

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
