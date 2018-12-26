{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Statistics.Classification.ConfusionMatrix.Types (
    ClassificationResult(..)
  , ClassificationScore(..)
  , sortByScore
  , scoreToResult
  ) where

import Data.Ord (comparing)

import Data.Csv ((.:), (.=), DefaultOrdered(..), ToNamedRecord(..), FromNamedRecord(..), namedRecord)
import Data.Vector.Algorithms.Tim
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as V

data ClassificationResult a = ClassificationResult {
    classifiedActual :: a
  , classifiedPredicted :: a
  } deriving (Eq, Show, Read, Ord)

data ClassificationScore a = ClassificationScore {
    classifiedExample :: !a
  , classifiedScore :: !Double
  } deriving (Eq, Show, Read, Ord)

instance Functor ClassificationScore where
  fmap f cs = cs { classifiedExample = f $ classifiedExample cs }

instance DefaultOrdered a => DefaultOrdered (ClassificationScore a) where
  headerOrder _ = headerOrder (undefined :: a) `V.snoc` "prob_up"

instance ToNamedRecord a => ToNamedRecord (ClassificationScore a) where
  toNamedRecord p = toNamedRecord (classifiedExample p) <> namedRecord ["prob_up" .= classifiedScore p]

instance FromNamedRecord a => FromNamedRecord (ClassificationScore a) where
  parseNamedRecord r = ClassificationScore <$>
        parseNamedRecord (HashMap.delete "prob_up" r)
    <*> r .: "prob_up"

sortByScore :: V.Vector (ClassificationScore a) -> V.Vector (ClassificationScore a)
sortByScore = V.modify (sortBy (comparing classifiedScore))

scoreToResult :: Double -> ClassificationScore Bool -> ClassificationResult Bool
scoreToResult cutoff (ClassificationScore a p) = ClassificationResult a (p > cutoff)
