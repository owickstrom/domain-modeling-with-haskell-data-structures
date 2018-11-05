module Reporting where

import           Data.Monoid (getSum)

import qualified Database    as DB
import           Project

-- start snippet report
data Report = Report
  { budgetProfit :: Money
  , netProfit    :: Money
  , difference   :: Money
  } deriving (Show, Eq)
-- end snippet report

-- start snippet monoid
instance Monoid Report where
  mempty = Report 0 0 0
  mappend (Report b1 n1 d1) (Report b2 n2 d2) =
    Report (b1 + b2) (n1 + n2) (d1 + d2)
-- end snippet monoid

-- start snippet calculateReport
calculateReport :: Budget -> [Transaction] -> Report
calculateReport budget transactions = Report
  { budgetProfit = budgetProfit'
  , netProfit    = netProfit'
  , difference   = netProfit' - budgetProfit'
  }
 where
  budgetProfit' = budgetIncome budget - budgetExpenditure budget
  netProfit'    = getSum (foldMap asProfit transactions)
  asProfit (Sale     m) = pure m
  asProfit (Purchase m) = pure (negate m)
-- end snippet calculateReport

-- start snippet calculateProjectReport
calculateProjectReport :: Project -> IO Report
calculateProjectReport project =
  case project of
    SingleProject p _ ->
      calculateReport
      <$> DB.getBudget p
      <*> DB.getTransactions p
    ProjectGroup _ projects ->
      foldMap calculateProjectReport projects
-- end snippet calculateProjectReport
