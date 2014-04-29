{-# LANGUAGE DeriveDataTypeable #-}

module Arbre.Expressions
(
 ObjectDef(..),
 Object(..),
 State(..),
 Def(..),
 Block(..),
 Expression(..),
 Literal(..),
 Environment(..),
 Context(..),
 Mapping(..),
 EffectType(..),
 EventType(..),
 ReceiverType(..),
 Program(..)
)
where

import Data.Typeable
import Data.Data
import Data.Map as M

import Arbre.Box
import Arbre.NativeTypes

data Interface = Interface [String]

data ObjectDef = ObjectDef [Def] deriving (Eq, Show, Typeable, Data)
data Def = Def String Expression deriving (Eq, Show, Typeable, Data)

data Object =
      Module ObjectDef
    | Object State ObjectDef
    deriving (Eq, Show, Typeable, Data)
data State =
      ObjectState Object
    | LiteralState Literal
    deriving (Eq, Show, Typeable, Data)
    
data Block = Block [String] Expression deriving (Eq, Show, Typeable, Data)

data Environment = Lex | Dyn | Self | Value | Local deriving (Eq, Show, Typeable, Data)

-- lex dyn self value local
data Context = Context Mapping Mapping Mapping Mapping Mapping deriving (Eq, Show, Typeable, Data)
data Mapping = Mapping (M.Map String Expression) deriving (Eq, Show, Typeable, Data)

data Expression =
    ObjectExp Object
  | Symref Environment String
  | Symdef String
  | Call Expression [Expression]
  | NativeCall NativeType [Expression]
  | Apply Expression [Expression]
  | BlockExp Block
  | Closure Mapping Mapping Mapping Mapping Block -- lex dyn self value
  | Mutation EffectType Expression Expression -- effect-type symdef value-expression
  | Event EventType Expression
  | Receiver ReceiverType Expression
  | Combine Expression Expression
  | ProgramExp Program
  | Error String
  deriving (Eq, Show, Typeable, Data)

data Program =
    PrintVal Expression
  | Emit Expression
  | Receive Expression Expression
  | EvalApply Expression Expression
  | Iterate Expression Program
  | Sequence Program Program
  deriving (Eq, Show, Typeable, Data)

data EffectType =
    Define
  | Set
  deriving (Eq, Show, Typeable, Data)

data EventType =
  Print
  deriving (Eq, Show, Typeable, Data)

data ReceiverType =
  Stdin
  deriving (Eq, Show, Typeable, Data)
