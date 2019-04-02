{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Statistics.Classification.Types (
    ClassificationResult(..)
  , ClassificationScore(..)
  , sortByScore
  , scoreToResult
  ) where

import Data.Ord (comparing)

import Data.Csv ((.:), (.=), DefaultOrdered(..), ToNamedRecord(..), FromNamedRecord(..), namedRecord)
import Data.Vector.Algorithms.Tim (sortBy)
import Test.QuickCheck (Arbitrary(..), choose)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

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

instance Arbitrary a => Arbitrary (ClassificationScore a) where
  arbitrary = ClassificationScore <$> arbitrary <*> choose (0, 1)


newtype instance VU.Unbox a => VU.MVector s (ClassificationScore a) = MV_ClassificationScore (VU.MVector s (a, Double))
newtype instance VU.Unbox a => VU.Vector (ClassificationScore a) = V_ClassificationScore (VU.Vector (a, Double))

instance VU.Unbox a => GM.MVector VU.MVector (ClassificationScore a) where
  basicLength (MV_ClassificationScore v) = GM.basicLength v
  basicUnsafeSlice i0 i1 (MV_ClassificationScore v) = MV_ClassificationScore $ GM.basicUnsafeSlice i0 i1 v
  basicOverlaps (MV_ClassificationScore v0) (MV_ClassificationScore v1) = GM.basicOverlaps v0 v1
  basicUnsafeNew n = MV_ClassificationScore <$> GM.basicUnsafeNew n
  basicInitialize (MV_ClassificationScore v) = GM.basicInitialize v
  basicUnsafeReplicate n (ClassificationScore a s) = MV_ClassificationScore <$> GM.basicUnsafeReplicate n (a, s)
  basicUnsafeRead (MV_ClassificationScore v) i = uncurry ClassificationScore <$> GM.basicUnsafeRead v i
  basicUnsafeWrite (MV_ClassificationScore v) i (ClassificationScore a s) = GM.basicUnsafeWrite v i (a, s)
  basicClear (MV_ClassificationScore v) = GM.basicClear v
  basicSet (MV_ClassificationScore v) (ClassificationScore a s) = GM.basicSet v (a, s)
  basicUnsafeCopy (MV_ClassificationScore v0) (MV_ClassificationScore v1) = GM.basicUnsafeCopy v0 v1
  basicUnsafeMove (MV_ClassificationScore v0) (MV_ClassificationScore v1) = GM.basicUnsafeMove v0 v1
  basicUnsafeGrow (MV_ClassificationScore v) = fmap MV_ClassificationScore . GM.basicUnsafeGrow v

instance VU.Unbox a => G.Vector VU.Vector (ClassificationScore a) where
  basicUnsafeFreeze (MV_ClassificationScore mv) = V_ClassificationScore <$> G.basicUnsafeFreeze mv
  basicUnsafeThaw (V_ClassificationScore v) = MV_ClassificationScore <$> G.basicUnsafeThaw v
  basicLength (V_ClassificationScore v) = G.basicLength v
  basicUnsafeSlice i0 i1 (V_ClassificationScore v) = V_ClassificationScore $ G.basicUnsafeSlice i0 i1 v
  basicUnsafeIndexM (V_ClassificationScore v) i = uncurry ClassificationScore <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_ClassificationScore mv) (V_ClassificationScore v) = G.basicUnsafeCopy mv v
  elemseq (V_ClassificationScore v) (ClassificationScore a s) = G.elemseq v (a, s)

instance VU.Unbox a => VU.Unbox (ClassificationScore a)

sortByScore :: V.Vector (ClassificationScore a) -> V.Vector (ClassificationScore a)
sortByScore = V.modify (sortBy (comparing classifiedScore))

scoreToResult :: Double -> ClassificationScore Bool -> ClassificationResult Bool
scoreToResult cutoff (ClassificationScore a p) = ClassificationResult a (p > cutoff)
