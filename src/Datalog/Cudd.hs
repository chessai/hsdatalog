{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Datalog.Cudd
  ( module Datalog.Cudd
    -- * Re-exports
  , SatBit
  ) where

import Control.Monad
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader, reader)
import Control.Monad.ST (ST, runST)
import Control.Monad.State.Class (MonadState, get, modify, put, state)
import Control.Monad.State.Strict (StateT, evalStateT)
import Cudd.Cudd (DDManager, DDNode, SatBit)
import Data.Foldable (foldlM)
import Data.Functor.Identity (Identity (..))
import Data.Map.Strict (Map)
import Data.Set (Set)
import Datalog.RelAlgebra (applyPermutation)
import Prelude hiding (and, not, or)

import qualified Cudd.Cudd as PureCudd
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector

-- KEEP YOUR MOUTH CLOSED WHEN YOU CHEW CUDD!
data BDD = BDD
  { bddNode :: !DDNode
  , bddVars :: !(Set Int)
  }
  deriving stock (Eq)

data Manager = Manager
  { managerManager :: !DDManager
  , managerNumVars :: !Int
  }

newtype CuddT m a = CuddT (StateT Manager m a)
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadState Manager)

type Cudd = CuddT Identity

chewT :: (Monad m) => CuddT m a -> m a
chewT (CuddT cudd) = evalStateT cudd (Manager PureCudd.cuddInit 0)

chew :: Cudd a -> a
chew = runIdentity . chewT

bdd :: DDNode -> BDD
bdd = flip BDD Set.empty

true :: (Monad m) => CuddT m BDD
true = bdd <$> state0 PureCudd.readOne

false :: (Monad m) => CuddT m BDD
false = bdd <$> state0 PureCudd.readLogicZero

ithVar :: (Monad m) => Int -> CuddT m BDD
ithVar i = do
  numVars <- managerNumVars <$> get
  modify (\m -> m { managerNumVars = max (i + 1) numVars })
  bdd <$> state1 PureCudd.ithVar i

and :: (Monad m) => BDD -> BDD -> CuddT m BDD
and x y = do
  ddnode <- state0 (\m -> PureCudd.bAnd m (bddNode x) (bddNode y))
  pure (BDD ddnode (bddVars x `Set.union` bddVars y))

or :: (Monad m) => BDD -> BDD -> CuddT m BDD
or x y = do
  ddnode <- state0 (\m -> PureCudd.bOr m (bddNode x) (bddNode y))
  pure (BDD ddnode (bddVars x `Set.union` bddVars y))

not :: (Monad m) => BDD -> CuddT m BDD
not x = bdd <$> state0 (\m -> PureCudd.bNot m (bddNode x))

nand :: (Monad m) => BDD -> BDD -> CuddT m BDD
nand x y = do
  ddnode <- state0 (\m -> PureCudd.bNand m (bddNode x) (bddNode y))
  pure (BDD ddnode (bddVars x `Set.union` bddVars y))

nor :: (Monad m) => BDD -> BDD -> CuddT m BDD
nor x y = do
  ddnode <- state0 (\m -> PureCudd.bNor m (bddNode x) (bddNode y))
  pure (BDD ddnode (bddVars x `Set.union` bddVars y))

xor :: (Monad m) => BDD -> BDD -> CuddT m BDD
xor x y = do
  ddnode <- state0 (\m -> PureCudd.bXor m (bddNode x) (bddNode y))
  pure (BDD ddnode (bddVars x `Set.union` bddVars y))

xnor :: (Monad m) => BDD -> BDD -> CuddT m BDD
xnor x y = do
  ddnode <- state0 (\m -> PureCudd.bXnor m (bddNode x) (bddNode y))
  pure (BDD ddnode (bddVars x `Set.union` bddVars y))

allSat :: (Monad m) => BDD -> CuddT m [[SatBit]]
allSat = state1 PureCudd.allSat . bddNode

exists :: (Monad m) => BDD -> BDD -> CuddT m BDD
exists x y = do
  ddnode <- state0 (\m -> PureCudd.bExists m (bddNode x) (bddNode y))
  pure (BDD ddnode (bddVars x `Set.difference` bddVars y))

forall :: (Monad m) => BDD -> BDD -> CuddT m BDD
forall x y = do
  ddnode <- state0 (\m -> PureCudd.bForall m (bddNode x) (bddNode y))
  pure (BDD ddnode (bddVars x `Set.difference` bddVars y))

permuteManager :: (Monad m) => BDD -> [Int] -> CuddT m BDD
permuteManager x y = bdd <$> state0 (\m -> PureCudd.permute m (bddNode x) y)

permute :: (Monad m) => BDD -> [Int] -> CuddT m BDD
permute node perm = do
  numVars <- managerNumVars <$> get
  let nodeVars = Set.toAscList (bddVars node)
  let go :: forall s. ST s [Int]
      go = do
        arr <- Vector.thaw $ Vector.fromList [0 .. numVars - 1]
        forM_ (zip nodeVars (applyPermutation perm nodeVars)) $ \(k,v) -> do
          MVector.write arr k v
        Vector.toList <$> Vector.unsafeFreeze arr
  permuteManager node (runST go)

restrictCube :: (Monad m) => BDD -> BDD -> CuddT m BDD
restrictCube x y = bdd <$> state0 (\m -> PureCudd.restrict m (bddNode x) (bddNode y))

restrict :: forall m. (Monad m) => BDD -> Map Int Bool -> CuddT m BDD
restrict node m = do
  trueNode <- true
  d <- traverse (uncurry ith) (Map.toList m) >>= foldlM and trueNode
  restrictCube node d
  where
    ith :: Int -> Bool -> CuddT m BDD
    ith i b = if b
      then ithVar i
      else not =<< ithVar i

state0 :: (MonadState Manager m)
  => (DDManager -> a)
  -> m a
state0 f = state $ \s -> (f (managerManager s), s)

state1 :: (MonadState Manager m)
  => (DDManager -> x -> a)
  -> x
  -> m a
state1 f x = state $ \s -> (f (managerManager s) x, s)

state2 :: (MonadState Manager m)
  => (DDManager -> x -> y -> a)
  -> x
  -> y
  -> m a
state2 f x y = state $ \s -> (f (managerManager s) x y, s)

{-
reader1 :: MonadReader r m => (r -> x -> a) -> x -> m a
reader1 f x = reader (flip f x)

reader2 :: MonadReader r m => (r -> x -> y -> a) -> x -> y -> m a
reader2 f x y = reader (\r -> f r x y)

reader3 :: MonadReader r m => (r -> x -> y -> z -> a) -> x -> y -> z -> m a
reader3 f x y z = reader (\r -> f r x y z)

reader4 :: MonadReader r m => (r -> x -> y -> z -> u -> a) -> x -> y -> z -> u -> m a
reader4 f x y z u = reader (\r -> f r x y z u)

readOne :: (Monad m) => CuddT m DDNode
readOne = reader PureCudd.readOne

readLogicZero :: (Monad m) => CuddT m DDNode
readLogicZero = reader PureCudd.readLogicZero

eval :: (Monad m) => DDNode -> [Int] -> CuddT m Bool
eval = reader2 PureCudd.eval

allSat :: (Monad m) => DDNode -> CuddT m [[SatBit]]
allSat = reader1 PureCudd.allSat

oneSat :: (Monad m) => DDNode -> CuddT m (Maybe [SatBit])
oneSat = reader1 PureCudd.oneSat

onePrime :: (Monad m) => DDNode -> DDNode -> CuddT m (Maybe [Int])
onePrime = reader2 PureCudd.onePrime

supportIndex :: (Monad m) => DDNode -> CuddT m [Bool]
supportIndex = reader1 PureCudd.supportIndex

ifThenElse :: (Monad m) => DDNode -> DDNode -> DDNode -> CuddT m DDNode
ifThenElse = reader3 PureCudd.bIte

swapVariables :: (Monad m) => DDNode -> [DDNode] -> [DDNode] -> CuddT m DDNode
swapVariables = reader3 PureCudd.swapVariables

indicesToCube :: (Monad m) => [Int] -> CuddT m DDNode
indicesToCube = reader1 PureCudd.indicesToCube

liCompaction :: (Monad m) => DDNode -> DDNode -> CuddT m DDNode
liCompaction = reader2 PureCudd.liCompaction

minimize :: (Monad m) => DDNode -> DDNode -> CuddT m DDNode
minimize = reader2 PureCudd.minimize

readSize :: (Monad m) => CuddT m Int
readSize = reader PureCudd.readSize

eq :: (Monad m) => [DDNode] -> [DDNode] -> CuddT m DDNode
eq = reader2 PureCudd.xEqY

gt :: (Monad m) => [DDNode] -> [DDNode] -> CuddT m DDNode
gt = reader2 PureCudd.xGtY

interval :: (Monad m) => [DDNode] -> Int -> Int -> CuddT m DDNode
interval = reader3 PureCudd.interval

disequality :: (Monad m) => Int -> Int -> [DDNode] -> [DDNode] -> CuddT m DDNode
disequality = reader4 PureCudd.disequality

inequality :: (Monad m) => Int -> Int -> [DDNode] -> [DDNode] -> CuddT m DDNode
inequality = reader4 PureCudd.inequality

pickOneMinterm :: (Monad m) => DDNode -> [DDNode] -> CuddT m (Maybe DDNode)
pickOneMinterm = reader2 PureCudd.pickOneMinterm

readPerm :: (Monad m) => Int -> CuddT m Int
readPerm = reader1 PureCudd.readPerm

readInvPerm :: (Monad m) => Int -> CuddT m Int
readInvPerm = reader1 PureCudd.readInvPerm

readPerms :: (Monad m) => CuddT m [Int]
readPerms = reader PureCudd.readPerms

readInvPerms :: (Monad m) => CuddT m [Int]
readInvPerms = reader PureCudd.readInvPerms

countMinterm :: (Monad m) => DDNode -> Int -> CuddT m Double
countMinterm = reader2 PureCudd.countMinterm

andAbstract :: (Monad m) => DDNode -> DDNode -> DDNode -> CuddT m DDNode
andAbstract = reader3 PureCudd.andAbstract

xorExistAbstract :: (Monad m) => DDNode -> DDNode -> DDNode -> CuddT m DDNode
xorExistAbstract = reader3 PureCudd.xorExistAbstract

makePrime :: (Monad m) => DDNode -> DDNode -> CuddT m DDNode
makePrime = reader2 PureCudd.makePrime

constrain :: (Monad m) => DDNode -> DDNode -> CuddT m DDNode
constrain = reader2 PureCudd.constrain

squeeze :: (Monad m) => DDNode -> DDNode -> CuddT m DDNode
squeeze = reader2 PureCudd.squeeze

largestCube :: (Monad m) => DDNode -> CuddT m (Int, DDNode)
largestCube = reader1 PureCudd.largestCube

lEq :: (Monad m) => DDNode -> DDNode -> CuddT m Bool
lEq = reader2 PureCudd.lEq
-}
