{-# LANGUAGE OverloadedStrings #-}

module PrettyPrint where

import qualified Data.Text   as Text
import           Data.Tree
import           Text.Printf

import           Project
import           Reporting

-- start snippet pretty-printing
asTree :: (a -> String) -> Project a -> Tree String
asTree prettyValue project =
  case project of
    SingleProject name x ->
      Node (printf "%s: %s" name (prettyValue x)) []
    ProjectGroup name projects ->
      Node (Text.unpack name) (map (asTree prettyValue) projects)

prettyProject :: (a -> String) -> Project a -> String
prettyProject prettyValue = drawTree . asTree prettyValue
-- end snippet pretty-printing

prettyReport :: Report -> String
prettyReport r =
  printf
    "Budget: %.2f, Net: %.2f, difference: %+.2f"
    (unMoney (budgetProfit r))
    (unMoney (netProfit r))
    (unMoney (difference r))
