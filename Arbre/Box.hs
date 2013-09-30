{-# LANGUAGE DeriveDataTypeable #-}

module Arbre.Box
(
    Literal(..),
)
where

import Data.Typeable
import Data.Data
import Data.Map as M

data Literal =
    StringLit String
  | IntegerLit Integer
  | BooleanLit Bool
  | FloatLit Float
  | ListLit [Literal]
  | MapLit [(Literal,Literal)]
  deriving (Eq, Show, Typeable, Data)      
