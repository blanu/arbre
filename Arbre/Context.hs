{-# LANGUAGE DeriveDataTypeable #-}

module Arbre.Context
(
  Context(..),
  empty,
  fromMap,
  resolve,
  addDynamicPair,
  lexChainWithMap,
  builtinContext
)
where

import Data.Typeable
import Data.Data
import qualified Data.Map as M

import Arbre.Program
import Arbre.Native

data Context = Context LexicalContext DynamicContext deriving (Eq, Show)
data LexicalContext = LexicalContext (M.Map String Expression) deriving (Eq, Show)
data DynamicContext = DynamicContext (M.Map String Expression) deriving (Eq, Show)

builtinContext :: Context
builtinContext = fromMap $ wrapNatives builtins

lexChainWithMap :: Context -> (M.Map String Expression) -> Context
lexChainWithMap (Context (LexicalContext lex) dyn) newLex =
  Context (LexicalContext (M.union lex newLex)) $ dyn

empty :: Context
empty = Context (LexicalContext M.empty) (DynamicContext M.empty)

fromList :: [(String,Expression)] -> Context
fromList list = Context (LexicalContext $ M.fromList list) (DynamicContext M.empty)

fromMap :: (M.Map String Expression) -> Context
fromMap map = Context (LexicalContext map) (DynamicContext M.empty)

resolve :: String -> Context -> Expression
resolve key (Context (LexicalContext lex) (DynamicContext dyn)) =
  let deref = M.lookup key lex
  in case deref of
    Just value -> value
    Nothing    ->
      let dynref = M.lookup key dyn
      in case dynref of
        Just value -> value
        Nothing -> Error $ "Undefined reference " ++ key

addDynamicPair :: Context -> (String, Expression) -> Context
addDynamicPair (Context lex (DynamicContext dyn)) (key, value) =
  Context lex (DynamicContext $ M.insert key value dyn)
