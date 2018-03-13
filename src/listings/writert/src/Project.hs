{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Project where

import           Data.Decimal (Decimal)
import           Data.Text    (Text)

newtype Money = Money
  { unMoney :: Decimal
  } deriving (Show, Eq, Num)

newtype ProjectId = ProjectId
  { unProjectId :: Int
  } deriving (Show, Eq, Num)

-- start snippet project
data Project g a
  = SingleProject Text
            a
  | ProjectGroup Text
                 g
                 [Project g a]
  deriving (Show, Eq, Functor, Foldable, Traversable)
-- end snippet project

data Budget = Budget
  { budgetIncome      :: Money
  , budgetExpenditure :: Money
  } deriving (Show, Eq)

data Transaction
  = Sale Money
  | Purchase Money
  deriving (Eq, Show)
