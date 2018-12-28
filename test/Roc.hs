{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Roc (testRoc) where

import Data.Csv ((.:), FromNamedRecord(..), decodeByName)
import Codec.Compression.Lzma (decompress)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Statistics.Classification (ClassificationScore(..))
import Statistics.Classification.ROC (ROC(..), roc)
import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>))
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector as V

checkAUC :: V.Vector (ClassificationScore Bool) -> Bool
checkAUC v = 0 <= auc && auc <= 1
  where auc = rocAUC $ roc v

newtype CScore = CScore { unCS :: ClassificationScore Bool }

instance FromNamedRecord CScore where
  parseNamedRecord o = fmap CScore $ ClassificationScore <$> fmap (== ("up" :: String)) (o .: "label") <*> o .: "prob_up"

testExamples :: TestTree
testExamples = testGroup "examples" $ map (\p -> testCase p (ex p)) validp
  where
    validp = filter (/= "ri_czc-2") $ pth <$> inst <*> [1..5 :: Int]
    inst = ["b_dce", "j_dce", "ri_czc"]
    pth prod n = prod ++ "-" ++ show n
    ex b =
      let p = "data" </> b <.> "csv" <.> "xz"
      in doesFileExist p >>= \case
      True -> decodeByName . decompress <$> LB.readFile p >>= \case
        Left err -> assertFailure err
        Right (_, cs) -> assertBool "auc in range" $ checkAUC (V.map unCS cs)
      False -> assertBool "no file" True

propAucRange :: [ClassificationScore Bool] -> Property
propAucRange l = length l >= 1 ==> checkAUC (V.fromList l)

testRoc :: TestTree
testRoc = testGroup "roc" [
    testProperty "auc range" propAucRange
  , testExamples
  ]
