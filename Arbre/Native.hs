module Arbre.Native
(
  builtins,
  wrapNatives,
  numdef
)
where

import qualified Data.Map as M

import Arbre.Program
import Arbre.Box

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

numdef :: ObjectDef
numdef = ObjectDef [
        Def "+" (BlockExp $ Block ["x"] (NativeCall "+" [(Symref "value"), (Symref "x")])),
        Def "*" (BlockExp $ Block ["x"] (NativeCall "*" [(Symref "value"), (Symref "x")])),
        Def "==" (BlockExp $ Block ["x"] (NativeCall "==" [(Symref "value"), (Symref "x")]))
    ]

add :: Native
add params@(a:b:[]) = do
    let x = unboxInteger a
    let y = unboxInteger b
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> boxInteger (i+j) numdef
add params = integerError params

mult :: Native
mult params@(a:b:[]) = do
    let x = unboxInteger a
    let y = unboxInteger b
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> boxInteger (i*j) numdef
mult params = integerError params

eq :: Native
eq params@(a:b:[]) = do
    let x = unboxInteger a
    let y = unboxInteger b
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> boxBoolean (i==j) numdef
eq params = integerError params

wrapNatives :: (M.Map String Native) -> (M.Map String Expression)
wrapNatives natives = M.mapWithKey wrapNative natives

wrapNative :: String -> Native -> Expression
wrapNative key native =
  let maybeParams = M.lookup key builtinParams
  in case maybeParams of
    Just params -> BlockExp $ Block params $ NativeCall key $ map Symref params
    Nothing     -> Error $ "No params for " ++ key
