{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances #-}

module Arbre.NativeTypes
(
  NativeType(..)
)
where

import Data.Typeable
import Data.Data

data NativeType =
    IntegerAdd
  | IntegerSubtract
  | IntegerMultiply
  | IntegerDivide
  | IntegerEquals
  | IntegerGreaterThan
  | IntegerLessThan
  | FloatAdd
  | FloatSubtract
  | FloatMultiply
  | FloatDivide
  | FloatEquals
  | FloatGreaterThan
  | FloatLessThan
  | StringEquals
  | BooleanEquals
  | If
  | And
  | Or
  | Append
  | Not deriving (Eq, Show, Typeable, Data, Ord)
