{-# LANGUAGE OverloadedStrings #-}

module Statistics.Classification.ROC (
    ROC(..)
  , ROCPoint(..)
  , rocPoint
  , efficientROC
  , roc
  ) where

import Data.List (foldl')

import Data.Csv ((.:), (.=), DefaultOrdered(..), FromNamedRecord(..), ToNamedRecord(..), header, namedRecord)
import qualified Data.Vector as V

import Statistics.Classification.ConfusionMatrix (
    ConfusionMatrix, ClassificationResult(..), ClassificationScore(..)
  , trueConfMatrix, truePosRate, falsePosRate, sortByScore
  )

data ROC = ROC {
    rocAUC :: !Double
  , rocCurve :: !(V.Vector ROCPoint)
  } deriving (Eq, Show, Read)

data ROCPoint = ROCPoint {
    rocTruePos :: {-# UNPACK #-}!Double
  , rocFalsePos :: {-# UNPACK #-}!Double
  , rocCutoff :: {-# UNPACK #-}!Double
  , rocConfusion :: !ConfusionMatrix
  } deriving (Eq, Show, Read)

rocPoint :: Double -> ConfusionMatrix -> ROCPoint
rocPoint cutoff cm = ROCPoint {
    rocTruePos = truePosRate cm
  , rocFalsePos = falsePosRate cm
  , rocCutoff = cutoff
  , rocConfusion = cm
  }

instance DefaultOrdered ROCPoint where
  headerOrder _ = header [
      "true_positive_rate"
    , "false_positive_rate"
    , "cutoff"
    , "f1"
    ] <> headerOrder (undefined :: ConfusionMatrix)

instance ToNamedRecord ROCPoint where
  toNamedRecord rp = namedRecord [
      "true_positive_rate" .= rocTruePos rp
    , "false_positive_rate" .= rocFalsePos rp
    , "cutoff" .= rocCutoff rp
    ] <> toNamedRecord (rocConfusion rp)

instance FromNamedRecord ROCPoint where
  parseNamedRecord r = ROCPoint <$>
        r .: "true_positive_rate"
    <*> r .: "false_positive_rate"
    <*> r .: "cutoff"
    <*> parseNamedRecord r

data EffRocState = EffRocState {
    effRocCutoff :: {-# UNPACK #-}!Double
  , effRocTP :: {-# UNPACK #-}!Int
  , effRocFP :: {-# UNPACK #-}!Int
  , effRocTPprev :: {-# UNPACK #-}!Int
  , effRocFPprev :: {-# UNPACK #-}!Int
  , effRocOut :: ![ROCPoint]
  , effRocAUC :: {-# UNPACK #-}!Double
  } deriving (Eq, Show, Read)

emptyEffRocState :: EffRocState
emptyEffRocState = EffRocState inf 0 0 0 0 [] 0
  where inf = 1 / 0

trapezoidArea :: Int -> Int -> Int -> Int -> Double
trapezoidArea x1 x2 y1 y2 = base * height
  where
    base = fromIntegral . abs $ x2 - x1
    height = 0.5 * fromIntegral y2 + 0.5 * fromIntegral y1

addArea :: EffRocState -> EffRocState
addArea s0 = s0 { effRocAUC = effRocAUC s0 + area }
  where
    area = trapezoidArea (effRocFP s0) (effRocFPprev s0) (effRocTP s0) (effRocTPprev s0)

updRocState :: Int -> Int -> EffRocState -> ClassificationScore Bool -> EffRocState
updRocState p n s0 r
  | effRocCutoff s0 /= cutoff = (addArea s1) { effRocCutoff = cutoff, effRocTPprev = effRocTP s1, effRocFPprev = effRocFP s1 }
  | otherwise = s1
  where
    cutoff = classifiedScore r
    actual = classifiedExample r
    s1' = if actual
      then s0 { effRocTP = effRocTP s0 + 1 }
      else s0 { effRocFP = effRocFP s0 + 1 }
    cm = trueConfMatrix (ClassificationResult p (effRocTP s1')) (ClassificationResult n (n - effRocFP s1'))
    s1 = s1' { effRocOut = rocPoint cutoff cm : effRocOut s1' }

-- O(n log n) ROC curve generation and AUC calculation
-- Algorithm 1 and 2 in
-- Fawcett, T., 2006. An Introduction to ROC Analysis. Pattern Recognition Letters 27 (861 - 874)
efficientROC :: Foldable f => Int -> Int -> f (ClassificationScore Bool) -> ROC
efficientROC p n = out . addArea . foldl' (updRocState p n) emptyEffRocState
  where
    finAUC effr
      | p == 0 || n == 0 = 0 -- degenerate case, no actual positive or negative examples.
      | otherwise = (effRocAUC effr + trapezoidArea n (effRocFPprev effr) n (effRocTPprev effr)) / fromIntegral (p * n)
    out s = ROC {
        rocAUC = finAUC s
      , rocCurve = V.fromList $ effRocOut s
      }

roc :: V.Vector (ClassificationScore Bool) -> ROC
roc v = efficientROC p n . V.reverse $ sortByScore v
  where
    p = V.length $ V.findIndices classifiedExample v
    n = V.length v - p
