{-# LANGUAGE OverloadedStrings #-}

module Statistics.Classification.ConfusionMatrix.Binary (
    ConfusionMatrix
  , trueConfMatrix
  , resultCount, classCount, totalCount
  , actualPositive, actualNegative, predPositive, predNegative
  , truePosRate, falseNegRate, falsePosRate, trueNegRate
  , prevalence, accuracy, errorRate, recall, precision, specificity
  , oddsRatio
  , phiCorrelation
  , fScore, f1Score
  , actualEntropy, predEntropy, condActualEntropy, condPredEntropy, totalCondEntropy
  , mutualInformation
  , asBinaryConfusion
  , asMultiConfusion
  ) where

import Data.Monoid (Sum(..))

import Data.Csv ((.:), (.=), DefaultOrdered(..), FromNamedRecord(..), ToNamedRecord(..), header, namedRecord)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Statistics.Classification.ConfusionMatrix.Multi (MultiConfusionMatrix(..), PosNeg
  , truePositiveCount, falsePositiveCount, falseNegativeCount, trueNegativeCount)
import Statistics.Classification.Types (ClassificationResult(..))

data ConfusionMatrix = ConfusionMatrix {
    truePositive :: {-# UNPACK #-}!(Sum Int)
  , falsePositive :: {-# UNPACK #-}!(Sum Int)
  , falseNegative :: {-# UNPACK #-}!(Sum Int)
  , trueNegative :: {-# UNPACK #-}!(Sum Int)
  } deriving (Eq, Show, Read)

instance Semigroup ConfusionMatrix where
  cm0 <> cm1 = ConfusionMatrix {
      truePositive  = truePositive  cm0 <> truePositive  cm1
    , falsePositive = falsePositive cm0 <> falsePositive cm1
    , falseNegative = falseNegative cm0 <> falseNegative cm1
    , trueNegative  = trueNegative  cm0 <> trueNegative  cm1
    }

instance Monoid ConfusionMatrix where
  mempty = ConfusionMatrix mempty mempty mempty mempty
  mappend = (<>)

instance DefaultOrdered ConfusionMatrix where
  headerOrder _ = header [
      "true_positive"
    , "false_positive"
    , "true_negative"
    , "false_negative"
    ]

instance ToNamedRecord ConfusionMatrix where
  toNamedRecord conf = namedRecord [
      "true_positive"  .= getSum (truePositive conf)
    , "false_positive" .= getSum (falsePositive conf)
    , "true_negative"  .= getSum (trueNegative conf)
    , "false_negative" .= getSum (falseNegative conf)
    ]

instance FromNamedRecord ConfusionMatrix where
  parseNamedRecord r = ConfusionMatrix <$>
        fmap Sum (r .: "true_positive")
    <*> fmap Sum (r .: "false_positive")
    <*> fmap Sum (r .: "false_negative")
    <*> fmap Sum (r .: "true_negative")

-- create confusion matrix given assumed true results
-- i.e., classifiedPredicted is the number predicted correctly
--       classifiedActual    is the number of actual samples in class
-- Give numbers for both true and false labels
trueConfMatrix :: ClassificationResult Int -> ClassificationResult Int -> ConfusionMatrix
trueConfMatrix pos neg = ConfusionMatrix {
    truePositive  = Sum $ classifiedPredicted pos
  , falseNegative = Sum $ classifiedActual pos - classifiedPredicted pos
  , falsePositive = Sum $ classifiedActual neg - classifiedPredicted neg
  , trueNegative  = Sum $ classifiedPredicted neg
  }

resultCount :: ConfusionMatrix -> ClassificationResult Bool -> Int
resultCount cm (ClassificationResult actual prd) = classCount cm prd actual

classCount :: ConfusionMatrix -> Bool -> Bool -> Int
classCount cm prd actual = getSum $ if prd
  then (if actual then  truePositive cm else falsePositive cm)
  else (if actual then falseNegative cm else  trueNegative cm)

totalCount :: ConfusionMatrix -> Int
totalCount cm = getSum $ truePositive cm <> falsePositive cm <> falseNegative cm <> trueNegative cm

actualPositive, actualNegative, predPositive, predNegative :: ConfusionMatrix -> Int
actualPositive cm = getSum $  truePositive cm <> falseNegative cm
actualNegative cm = getSum $ falsePositive cm <>  trueNegative cm
predPositive   cm = getSum $  truePositive cm <> falsePositive cm
predNegative   cm = getSum $ falseNegative cm <>  trueNegative cm

truePosRate, falseNegRate, falsePosRate, trueNegRate :: ConfusionMatrix -> Double
truePosRate = recall
falseNegRate cm = fromIntegral (getSum (falseNegative cm)) / fromIntegral (actualPositive cm)
falsePosRate cm = fromIntegral (getSum (falsePositive cm)) / fromIntegral (actualNegative cm)
trueNegRate = specificity

prevalence, accuracy, errorRate, recall, precision, specificity :: ConfusionMatrix -> Double
prevalence cm = fromIntegral (actualPositive cm) / fromIntegral (totalCount cm)
accuracy cm = fromIntegral (getSum (truePositive cm <> trueNegative cm)) / fromIntegral (totalCount cm)
errorRate cm = fromIntegral (getSum (falsePositive cm <> falseNegative cm)) / fromIntegral (totalCount cm)
recall cm = fromIntegral (getSum (truePositive cm)) / fromIntegral (actualPositive cm)
precision cm = fromIntegral (getSum (truePositive cm)) / fromIntegral (predPositive cm)
specificity cm = fromIntegral (getSum (trueNegative cm)) / fromIntegral (actualNegative cm)

oddsRatio :: ConfusionMatrix -> Double
oddsRatio (ConfusionMatrix (Sum tp) (Sum fp) (Sum fn) (Sum tn)) =
  fromIntegral (tp * tn) / fromIntegral (fn * fp)

phiCorrelation :: ConfusionMatrix -> Double
phiCorrelation (ConfusionMatrix (Sum tp) (Sum fp) (Sum fn) (Sum tn)) =
  fromIntegral (tp * tn - fp * fn) / sqrt (fromIntegral $ (tp + fn) * (tn + fp) * (tp + fp) * (tn + fn))

fScore :: Double -> ConfusionMatrix -> Double
fScore alpha cm = recip $ alpha * recip (precision cm) + (1 - alpha) * recip (recall cm)

f1Score :: ConfusionMatrix -> Double
f1Score = fScore 0.5

-- Entropy calculations
binomEntropy :: Double -> Double
binomEntropy p = p * log p + (1 - p) * log (1 - p)

discreteEntropy :: Int -> Int -> Double
discreteEntropy n tot = binomEntropy (fromIntegral n / fromIntegral tot)

actualEntropy, predEntropy :: ConfusionMatrix -> Double
actualEntropy cm = discreteEntropy (actualPositive cm) (totalCount cm)
predEntropy cm = discreteEntropy (predPositive cm) (totalCount cm)

condActualEntropy :: ConfusionMatrix -> Bool -> Double
condActualEntropy cm plbl = if plbl
  then discreteEntropy (getSum (truePositive cm)) (predPositive cm)
  else discreteEntropy (getSum (trueNegative cm)) (predNegative cm)

condPredEntropy :: ConfusionMatrix -> Bool -> Double
condPredEntropy cm albl = if albl
  then discreteEntropy (getSum (truePositive cm)) (actualPositive cm)
  else discreteEntropy (getSum (trueNegative cm)) (actualNegative cm)

totalCondEntropy :: ConfusionMatrix -> Double
totalCondEntropy cm = ppos * condActualEntropy cm True + (1 - ppos) * condActualEntropy cm False
  where ppos = fromIntegral (predPositive cm) / fromIntegral (totalCount cm)

mutualInformation :: ConfusionMatrix -> Double
mutualInformation cm = actualEntropy cm - totalCondEntropy cm

asBinaryConfusion :: Ord a => PosNeg a -> MultiConfusionMatrix a -> ConfusionMatrix
asBinaryConfusion posneg mcm = ConfusionMatrix {
    truePositive = Sum $ truePositiveCount mcm posneg
  , falsePositive = Sum $ falsePositiveCount mcm posneg
  , falseNegative = Sum $ falseNegativeCount mcm posneg
  , trueNegative = Sum $ trueNegativeCount mcm posneg
  }

asMultiConfusion :: ConfusionMatrix -> MultiConfusionMatrix Bool
asMultiConfusion cm = MultiConfusionMatrix {
    confCounts = Map.fromList [
        (ClassificationResult True  True,  getSum (truePositive cm))
      , (ClassificationResult False True,  getSum (falsePositive cm))
      , (ClassificationResult True  False, getSum (falseNegative cm))
      , (ClassificationResult False False, getSum (trueNegative cm))
      ]
  , confClasses = Set.fromList [True, False]
  }
