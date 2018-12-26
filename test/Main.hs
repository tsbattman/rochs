
module Main (main) where

import Test.Tasty

import Roc

main :: IO ()
main = defaultMain $ testGroup "all tests" [
    testRoc
  ]
