{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
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
data Project a
  = SingleProject Text
                  a
  | ProjectGroup Text
                 [Project a]
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
