module Arbre.Native
(
  builtins,
  wrapNatives
)
where

import qualified Data.Map as M

import Arbre.Program

type Native = ([Expression] -> Expression)

builtins :: (M.Map String Native)
builtins = M.fromList [
    ("+", add),
    ("*", mult),
    ("==", eq)
  ]

builtinParams :: (M.Map String [String])
builtinParams = M.fromList [
    ("+", ["a", "b"]),
    ("*", ["a", "b"]),
    ("==", ["a", "b"])
  ]

add :: Native
add ((LiteralExp (IntegerLit a)):(LiteralExp (IntegerLit b)):[]) =
  LiteralExp $ IntegerLit $ a+b
add params = Error $ "Type error, not integer literals" ++ (show params)

mult :: Native
mult ((LiteralExp (IntegerLit a)):(LiteralExp (IntegerLit b)):[]) =
  LiteralExp $ IntegerLit $ a*b
mult params = Error $ "Type error, not integer literals" ++ (show params)

eq :: Native
eq ((LiteralExp (IntegerLit a)):(LiteralExp (IntegerLit b)):[]) =
  LiteralExp $ BooleanLit $ a==b
eq params = Error $ "Type error, not integer literals" ++ (show params)

wrapNatives :: (M.Map String Native) -> (M.Map String Expression)
wrapNatives natives = M.mapWithKey wrapNative natives

wrapNative :: String -> Native -> Expression
wrapNative key native =
  let maybeParams = M.lookup key builtinParams
  in case maybeParams of
    Just params -> BlockExp $ Block params $ NativeCall key $ map Symref params
    Nothing     -> Error $ "No params for " ++ key
