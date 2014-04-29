{-# LANGUAGE DeriveDataTypeable #-}

module Arbre.Context
(
  Computation(..),
  empty,
  emptyContext,
  resolve,
  addDynamicPair,
  chainMapping,
  unwrapDef,
  bindPair,
  stack,
  close,
  open
)
where

import Data.Typeable
import Data.Data
import qualified Data.Map as M

import Arbre.Expressions

data Computation = Computation Context Expression deriving (Eq, Show)

unwrapDef :: Def -> (String, Expression)
unwrapDef (Def name exp) = (name,exp)

chainMapping :: Mapping -> Mapping -> Mapping
--chainMapping (Mapping lex) (Mapping newLex) = Mapping $ M.union lex newLex
chainMapping (Mapping lex) (Mapping newLex) = Mapping $ M.union newLex lex

empty :: Mapping
empty = Mapping M.empty

emptyContext :: Context
emptyContext = Context empty empty empty empty empty

resolve :: Environment -> String -> Context -> Expression
resolve Lex   key (Context lex dyn self value local) = maplookup key lex
resolve Dyn   key (Context lex dyn self value local) = maplookup key dyn
resolve Self  key (Context lex dyn self value local) = maplookup key self
resolve Value key (Context lex dyn self value local) = maplookup key value
resolve Local key (Context lex dyn self value local) = maplookup key local

maplookup :: String -> Mapping -> Expression
maplookup key (Mapping map) =
  let ref = M.lookup key map
    in case ref of
      Just value -> value
      Nothing -> Error $ "Undefined reference " ++ key ++": " ++ show (M.keys map)

bind :: Environment -> String -> Expression -> Context -> Context
bind Lex   key val (Context lex dyn self value local) = Context (bindMapping key val lex) dyn self value local
bind Dyn   key val (Context lex dyn self value local) = Context lex (bindMapping key val dyn) self value local
bind Self  key val (Context lex dyn self value local) = Context lex dyn (bindMapping key val self) value local
bind Value key val (Context lex dyn self value local) = Context lex dyn self (bindMapping key val value) local
bind Local key val (Context lex dyn self value local) = Context lex dyn self value (bindMapping key val local)

bindMapping :: String -> Expression -> Mapping -> Mapping
bindMapping key value (Mapping map) = Mapping $ M.insert key value map

bindPair :: Environment -> Context -> (String, Expression) -> Context
bindPair Lex   (Context lex dyn self value local) (key, val) = Context (bindPairMapping (key, val) lex) dyn self value local
bindPair Dyn   (Context lex dyn self value local) (key, val) = Context lex (bindPairMapping (key, val) dyn) self value local
bindPair Self  (Context lex dyn self value local) (key, val) = Context lex dyn (bindPairMapping (key, val) self) value local
bindPair Value (Context lex dyn self value local) (key, val) = Context lex dyn self (bindPairMapping (key, val) value) local
bindPair Local (Context lex dyn self value local) (key, val) = Context lex dyn self value (bindPairMapping (key, val) local)

bindPairMapping :: (String, Expression) -> Mapping -> Mapping
bindPairMapping (key, value) (Mapping map) = Mapping $ M.insert key value map

addDynamicPair :: Mapping -> (String, Expression) -> Mapping
addDynamicPair (Mapping dyn) (key, value) = Mapping $ M.insert key value dyn

stack :: Context -> Context
stack (Context lex dyn self value local) = Context (chainMapping lex local) dyn self value empty

close :: Context -> Expression -> Expression
close (Context lex dyn self value local) (BlockExp block) = Closure (chainMapping lex local) dyn self value block

open :: Expression -> Context
open (Closure lex dyn self value _) = Context lex dyn self value empty
