{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Project where

import           Data.Text (Text)

newtype Money = Money
  { unMoney :: Double
  } deriving (Show, Eq, Num)

newtype ProjectId = ProjectId
  { unProjectId :: Int
  } deriving (Show, Eq, Num)

-- start snippet project
data Project
  = SingleProject ProjectId
                  Text
  | ProjectGroup Text
                 [Project]
  deriving (Show, Eq)
-- end snippet project

-- start snippet budget
data Budget = Budget
  { budgetIncome      :: Money
  , budgetExpenditure :: Money
  } deriving (Show, Eq)
-- end snippet budget

-- start snippet transaction
data Transaction
  = Sale Money
  | Purchase Money
  deriving (Eq, Show)
-- end snippet transaction
