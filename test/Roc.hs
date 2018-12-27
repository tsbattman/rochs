
module Roc (testRoc) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Statistics.Classification.ROC (ROC(..), roc)
import qualified Data.Vector as V

propAucRange :: Gen Bool
propAucRange = do
  l <- arbitrary `suchThat` \l -> length l > 1
  let auc = rocAUC . roc $ V.fromList l
  return $ 0 <= auc && auc <= 1

testRoc :: TestTree
testRoc = testGroup "roc" [
    testProperty "auc range" propAucRange
  ]
