module Datalog.ID3 where

import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Ord (comparing)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Safe

data DecisionTree k v cls
  = DTLeaf !cls
  | DTBranch !k !(Map v (DecisionTree k v cls))

type DataPoint k v cls = (Map k v, cls)

type DataSet k v cls = [DataPoint k v cls]

id3 :: (Ord cls, Ord k, Ord v) => DataSet k v cls -> DecisionTree k v cls
id3 ds = case Safe.minimumByMay (comparing entropyOf) (attrsOf ds) of
           Nothing -> DTLeaf $ assertAllEqual (map snd ds)
           Just k  -> DTBranch k
                      $ Map.fromListWith undefined
                      $ map (\v -> (v, id3 (conditionOn k v ds)))
                      $ attrValues ds k

  where
    entropyOf k =
      sum ((\v -> attrEntropy (conditionOn k v ds)) <$> attrValues ds k)

attrsOf :: (Ord k, Ord v) => DataSet k v cls -> [k]
attrsOf ds = toList $ Set.unions $ map (Map.keysSet . fst) ds

attrValues :: (Ord k, Ord v) => DataSet k v cls -> k -> [v]
attrValues ds k = setNub $ mapMaybe (Map.lookup k . fst) ds

conditionOn :: (Ord k, Eq v) => k -> v -> DataSet k v cls -> DataSet k v cls
conditionOn k v = map (first (Map.delete k))
                  . filter ((== Just v) . Map.lookup k . fst)

attrEntropy :: (Ord cls) => DataSet k v cls -> Double
attrEntropy ds =
  sum
  $ map (\cls -> negate (let p = probabilityOf ds cls in p * log2 p))
  $ setNub
  $ map snd ds
  where
    log2 = logBase 2.0

probabilityOf :: (Eq cls) => DataSet k v cls -> cls -> Double
probabilityOf ds cls = fromIntegral (length (filter ((== cls) . snd) ds))
                       / fromIntegral (length ds)

assertAllEqual :: (Foldable t, Eq a) => t a -> a
assertAllEqual = go . toList
  where
    go []     = error "assertAllEqual: empty list"
    go (x:xs) = if all (== x) xs
                then x
                else error "assertAllEqual: not all equal"

setNub :: (Ord a) => [a] -> [a]
setNub = Set.toList . Set.fromList
