
module Statistics.Classification.ConfusionMatrix.Multi (
    MultiConfusionMatrix(..)
  , multiClassCount
  , multiTotalCount
  , truePositiveCount, falsePositiveCount, falseNegativeCount, trueNegativeCount
  , confusionMatrix
  , PosNeg
  , posVal, negVal, posVals, negVals
  , trueVal, falseVal
  , isPos, isNeg
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Statistics.Classification.ConfusionMatrix.Types (ClassificationResult(..))

data MultiConfusionMatrix a = MultiConfusionMatrix {
    confCounts :: Map.Map (ClassificationResult a) Int
  , confClasses :: Set.Set a
  } deriving (Eq, Show, Read)

instance Ord a => Semigroup (MultiConfusionMatrix a) where
  (MultiConfusionMatrix mcm0 mcc0) <> (MultiConfusionMatrix mcm1 mcc1) =
    MultiConfusionMatrix (Map.unionWith (+) mcm0 mcm1) (Set.union mcc0 mcc1)

instance Ord a => Monoid (MultiConfusionMatrix a) where
  mempty = MultiConfusionMatrix Map.empty Set.empty
  mappend = (<>)

multiClassCount :: Ord a => MultiConfusionMatrix a -> ClassificationResult a -> Int
multiClassCount (MultiConfusionMatrix conf _) r = Map.findWithDefault 0 r conf

truePositiveCount,falsePositiveCount,falseNegativeCount,trueNegativeCount :: Ord a => MultiConfusionMatrix a -> PosNeg a -> Int
truePositiveCount (MultiConfusionMatrix conf _) posneg = sum $
  Map.filterWithKey (\(ClassificationResult a p) _-> isPos posneg a && isPos posneg p) conf
falsePositiveCount (MultiConfusionMatrix conf _) posneg = sum $
  Map.filterWithKey (\(ClassificationResult a p) _-> isNeg posneg a && isPos posneg p) conf
falseNegativeCount (MultiConfusionMatrix conf _) posneg = sum $
  Map.filterWithKey (\(ClassificationResult a p) _-> isPos posneg a && isNeg posneg p) conf
trueNegativeCount (MultiConfusionMatrix conf _) posneg = sum $
  Map.filterWithKey (\(ClassificationResult a p) _-> isNeg posneg a && isNeg posneg p) conf

multiTotalCount :: MultiConfusionMatrix a -> Int
multiTotalCount = sum . confCounts

confusionMatrix :: Ord a => [ClassificationResult a] -> MultiConfusionMatrix a
confusionMatrix = foldMap go
  where
    go k = MultiConfusionMatrix {
        confCounts = Map.singleton k 1
      , confClasses = Set.fromList [classifiedActual k, classifiedPredicted k]
      }

data PosNeg a = PosVal (Set.Set a) | NegVal (Set.Set a)
  deriving (Eq, Show, Read)

posVal,negVal :: a -> PosNeg a
posVal = PosVal . Set.singleton
negVal = NegVal . Set.singleton

posVals,negVals :: Ord a => [a] -> PosNeg a
posVals = PosVal . Set.fromList
negVals = NegVal . Set.fromList

trueVal,falseVal :: PosNeg Bool
trueVal = posVal True
falseVal = posVal False

isPos,isNeg :: Ord a => PosNeg a -> a -> Bool
isPos (PosVal pos) v = Set.member v pos
isPos (NegVal neg) v = Set.notMember v neg
isNeg (PosVal pos) v = Set.notMember v pos
isNeg (NegVal neg) v = Set.member v neg
