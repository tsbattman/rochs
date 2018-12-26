{-# LANGUAGE OverloadedStrings #-}

module Reporting.Component.Classification.ConfusionMatrix.Html (
    confusionMatrixHtml
  ) where

import Colonnade
import Text.Blaze.Colonnade (Cell(..), encodeCellTable, htmlCell)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Data.Set as Set

import Reporting.Component.Classification.ConfusionMatrix.Multi
import Reporting.Component.Classification.ConfusionMatrix.Types

confusionMatrixHtml :: Ord a => (a -> Html) -> MultiConfusionMatrix a -> Html
confusionMatrixHtml f conf = encodeCellTable tblAttr tbl $ Set.toList cls
  where
    cls = confClasses conf
    tblAttr = class_ "table table-bordered table-hover table-striped"
    tbl = headed "pred" (htmlCell . f) <> foldMap cfCell cls
    cfCell act = headed (htmlCell $ f act)
      (Cell (class_ "text-right") . toHtml . multiClassCount conf . ClassificationResult act)
