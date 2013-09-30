{-# LANGUAGE DeriveDataTypeable #-}

module Arbre.Program
(
 ObjectDef(..),
 Object(..),
 State(..),
 Def(..),
 Block(..),
 Expression(..),
 Literal(..),
 unboxInteger,
 boxInteger,
 integerError,
 unboxBoolean,
 boxBoolean,
 booleanError
)
where

import Data.Typeable
import Data.Data
import Data.Map as M

import Arbre.Box

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

data Expression =
    ObjectExp Object
  | Symref String
  | Symdef String
  | Call String [Expression]
  | NativeCall String [Expression]
  | Apply Block [Expression]
  | BlockExp Block
  | Error String
  deriving (Eq, Show, Typeable, Data)

unboxInteger :: Expression -> Maybe Integer
unboxInteger (ObjectExp (Object (LiteralState (IntegerLit i)) _)) = Just i
unboxInteger _ = Nothing

boxInteger :: Integer -> ObjectDef -> Expression
boxInteger i def = ObjectExp $ Object (LiteralState $ IntegerLit i) def

integerError :: [Expression] -> Expression
integerError params = Error $ "Type error, not integer literals" ++ (show params)

unboxBoolean :: Expression -> Maybe Bool
unboxBoolean (ObjectExp (Object (LiteralState (BooleanLit i)) _)) = Just i
unboxBoolean _ = Nothing
boxBoolean :: Bool -> ObjectDef -> Expression
boxBoolean i def = ObjectExp $ Object (LiteralState $ BooleanLit i) def

booleanError :: [Expression] -> Expression
booleanError params = Error $ "Type error, not boolean literals" ++ (show params)
