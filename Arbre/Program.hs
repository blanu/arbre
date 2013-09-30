{-# LANGUAGE DeriveDataTypeable #-}

module Arbre.Program
(
 ObjectDef(..),
 Def(..),
 Block(..),
 Expression(..),
 Literal(..),
)
where

import Data.Typeable
import Data.Data
import Data.Map as M

data ObjectDef = ObjectDef [Def] deriving (Eq, Show, Typeable, Data)
data Def = Def String Expression deriving (Eq, Show, Typeable, Data)

data Block = Block [String] Expression deriving (Eq, Show, Typeable, Data)

data Expression =
    LiteralExp Literal
  | Symref String
  | Symdef String
  | Call String [Expression]
  | NativeCall String [Expression]
  | Apply Block [Expression]
  | BlockExp Block
  | Error String
  deriving (Eq, Show, Typeable, Data)

data Literal =
    StringLit String
  | IntegerLit Integer
  | BooleanLit Bool
  | FloatLit Float
  | ListLit [Literal]
  | MapLit [(Literal,Literal)]
  deriving (Eq, Show, Typeable, Data)
