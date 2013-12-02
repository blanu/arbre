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
 Environment(..),
 Context(..),
 Mapping(..),
 EffectType(..),
 EventType(..),
 unboxInteger,
 unboxFloat,
 boxInteger,
 boxFloat,
 integerError,
 floatError,
 unboxBoolean,
 boxBoolean,
 booleanError,
 typeError
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

data Environment = Lex | Dyn | Self | Value | Local deriving (Eq, Show, Typeable, Data)

-- lex dyn self value local
data Context = Context Mapping Mapping Mapping Mapping Mapping deriving (Eq, Show, Typeable, Data)
data Mapping = Mapping (M.Map String Expression) deriving (Eq, Show, Typeable, Data)

data Expression =
    ObjectExp Object
  | Symref Environment String
  | Symdef String
  | Call Expression [Expression]
  | NativeCall String [Expression]
  | Apply Expression [Expression]
  | BlockExp Block
  | Closure Mapping Mapping Mapping Mapping Block -- lex dyn self value
  | Mutation EffectType Expression Expression -- effect-type symdef value-expression
  | Event EventType Expression
  | Combine Expression Expression
  | Error String
  deriving (Eq, Show, Typeable, Data)

data EffectType =
    Define
  | Set
  deriving (Eq, Show, Typeable, Data)

data EventType =
  Print
  deriving (Eq, Show, Typeable, Data)

unboxInteger :: Expression -> Maybe Integer
unboxInteger (ObjectExp (Object (LiteralState (IntegerLit i)) _)) = Just i
unboxInteger _ = Nothing

boxInteger :: Integer -> ObjectDef -> Expression
boxInteger i def = ObjectExp $ Object (LiteralState $ IntegerLit i) def

integerError :: [Expression] -> Expression
integerError params = Error $ "Type error, not integer literals" ++ (show params)

floatError :: [Expression] -> Expression
floatError params = Error $ "Type error, not float literals" ++ (show params)

unboxFloat :: Expression -> Maybe Float
unboxFloat (ObjectExp (Object (LiteralState (FloatLit i)) _)) = Just i
unboxFloat _ = Nothing

boxFloat :: Float -> ObjectDef -> Expression
boxFloat f def = ObjectExp $ Object (LiteralState $ FloatLit f) def

unboxBoolean :: Expression -> Maybe Bool
unboxBoolean (ObjectExp (Object (LiteralState (BooleanLit i)) _)) = Just i
unboxBoolean _ = Nothing

boxBoolean :: Bool -> ObjectDef -> Expression
boxBoolean i def = ObjectExp $ Object (LiteralState $ BooleanLit i) def

booleanError :: [Expression] -> Expression
booleanError params = Error $ "Type error, not boolean literals" ++ (show params)

typeError :: [Expression] -> Expression
typeError params = Error $ "General type error: " ++ (show params)
