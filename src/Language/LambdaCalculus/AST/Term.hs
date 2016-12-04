module Language.LambdaCalculus.AST.Term
  ( Info(..)
  , Term(..)
  ) where

data Term =
    TmVar Info Int Int
  | TmAbs Info String Term
  | TmApp Info Term Term
  deriving (Show)

data Info = Info { row :: Int, col :: Int } deriving (Show)
