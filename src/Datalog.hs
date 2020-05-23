{-# language
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , DerivingStrategies
  , LambdaCase
  , ScopedTypeVariables
#-}

module Datalog
  ( main
  ) where

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.ST (ST, runST, stToIO)
import Data.Foldable (foldl', foldlM)
import Data.Map.Strict (Map)
import Data.Void (Void)
import Datalog.Cudd (CuddT, DDNode)
import Datalog.Graph
import Datalog.CycleEnumeration
import Datalog.Stratification
import Datalog.Syntax
import qualified Data.Map.Strict as Map
import qualified Datalog.Cudd as Cudd

main :: IO ()
main = do
  let progFile = "test.datalog"
  progCode <- readFile progFile
  case parseProgram progFile progCode of
    Left err -> putStrLn err
    Right prog -> do
      mapM_ print prog
      print $ computeProgramPrecedenceGraph prog
      print $ enumerateWeightCycles $ computeProgramPrecedenceGraph prog
      print $ parityStratifyCheck prog
{-
  implies <- Cudd.chewT $ do
    v1 <- Cudd.ithVar 0
    v2 <- Cudd.ithVar 1
    conj <- Cudd.and v1 v2
    implies <- Cudd.lEq conj v1
    pure implies
  print implies
-}

type Constant = Int

data RelAlgebra attr rel
  = Join rel rel
  | Union rel rel
  | Project [attr] rel
  | Rename attr attr rel
  | Difference rel rel
  | Select attr Constant rel
  deriving stock (Eq, Show)
  deriving stock (Functor, Foldable, Traversable)

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
