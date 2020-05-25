{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Datalog.Cudd
  ( CuddT(..)
  , chewT, chewWithT

  , Cudd
  , chew, chewWith


  , ithVar

  , and, or, not, nand, nor, xor, xnor

  , permute

  , eq, gt

  , restrict
  , restrict'

  , andAbstract
  , xorExistAbstract
  , lEq

    -- * Re-exports
  , DDManager
  , DDNode
  ) where

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader, reader)
import Cudd.Cudd (DDManager, DDNode, SatBit)
import Data.Foldable (foldlM)
import Data.Functor.Identity (Identity (..))
import Data.Map.Strict (Map)
import Prelude hiding (and, not, or)

import qualified Cudd.Cudd as PureCudd
import qualified Data.Map.Strict as Map

newtype CuddT m a = CuddT (ReaderT DDManager m a)
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadReader DDManager)

type Cudd = CuddT Identity

chewWithT :: [Int] -> CuddT m a -> m a
chewWithT order (CuddT cudd) = runReaderT cudd (PureCudd.cuddInitOrder order)

chewT :: CuddT m a -> m a
chewT (CuddT cudd) = runReaderT cudd PureCudd.cuddInit

chew :: Cudd a -> a
chew = runIdentity . chewT

chewWith :: [Int] -> Cudd a -> a
chewWith order = runIdentity . chewWithT order

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

ithVar :: (Monad m) => Int -> CuddT m DDNode
ithVar = reader1 PureCudd.ithVar

and :: (Monad m) => DDNode -> DDNode -> CuddT m DDNode
and = reader2 PureCudd.bAnd

or :: (Monad m) => DDNode -> DDNode -> CuddT m DDNode
or = reader2 PureCudd.bOr

not :: (Monad m) => DDNode -> CuddT m DDNode
not = reader1 PureCudd.bNot

nand :: (Monad m) => DDNode -> DDNode -> CuddT m DDNode
nand = reader2 PureCudd.bNand

nor :: (Monad m) => DDNode -> DDNode -> CuddT m DDNode
nor = reader2 PureCudd.bNor

xor :: (Monad m) => DDNode -> DDNode -> CuddT m DDNode
xor = reader2 PureCudd.bXor

xnor :: (Monad m) => DDNode -> DDNode -> CuddT m DDNode
xnor = reader2 PureCudd.bXnor

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

exists :: (Monad m) => DDNode -> DDNode -> CuddT m DDNode
exists = reader2 PureCudd.bExists

forall :: (Monad m) => DDNode -> DDNode -> CuddT m DDNode
forall = reader2 PureCudd.bForall

ifThenElse :: (Monad m) => DDNode -> DDNode -> DDNode -> CuddT m DDNode
ifThenElse = reader3 PureCudd.bIte

permute :: (Monad m) => DDNode -> [Int] -> CuddT m DDNode
permute = reader2 PureCudd.permute

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

restrict :: (Monad m) => DDNode -> DDNode -> CuddT m DDNode
restrict = reader2 PureCudd.restrict

restrict' :: forall m. (Monad m) => DDNode -> Map Int Bool -> CuddT m DDNode
restrict' node m = do
  trueNode <- readOne
  d <- traverse (uncurry ith) (Map.toList m) >>= foldlM and trueNode
  restrict node d
  where
    ith :: Int -> Bool -> CuddT m DDNode
    ith i b = if b
      then ithVar i
      else not =<< ithVar i

squeeze :: (Monad m) => DDNode -> DDNode -> CuddT m DDNode
squeeze = reader2 PureCudd.squeeze

largestCube :: (Monad m) => DDNode -> CuddT m (Int, DDNode)
largestCube = reader1 PureCudd.largestCube

lEq :: (Monad m) => DDNode -> DDNode -> CuddT m Bool
lEq = reader2 PureCudd.lEq
