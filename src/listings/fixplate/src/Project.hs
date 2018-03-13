{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Project where

import           Data.Decimal                (Decimal)
import           Data.Generics.Fixplate.Base (Mu (Fix))
import           Data.Text                   (Text)

newtype Money = Money
  { unMoney :: Decimal
  } deriving (Show, Eq, Num)

newtype ProjectId = ProjectId
  { unProjectId :: Int
  } deriving (Show, Eq, Num)

-- start snippet Project
data ProjectF f
  = SingleProject ProjectId
                  Text
  | ProjectGroup Text
                 [f]
  deriving (Show, Eq, Functor, Foldable, Traversable)

type Project = Mu ProjectF
-- end snippet Project

-- start snippet constructors
singleProject :: ProjectId -> Text -> Project
singleProject p = Fix . SingleProject p

projectGroup :: Text -> [Project] -> Project
projectGroup name = Fix . ProjectGroup name
-- end snippet constructors

data Budget = Budget
  { budgetIncome      :: Money
  , budgetExpenditure :: Money
  } deriving (Show, Eq)

data Transaction
  = Sale Money
  | Purchase Money
  deriving (Eq, Show)
