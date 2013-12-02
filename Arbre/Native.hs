module Arbre.Native
(
  builtins,
  wrapNatives,
  numdef,
  builtinContext,
  builtinEnvironment,
  objectContext
)
where

import qualified Data.Map as M
import Data.Typeable
import Data.Data

import Arbre.Program
import Arbre.Box
import Arbre.Context

type Native = ([Expression] -> Expression)

builtinContext :: Context
builtinContext = Context builtinEnvironment empty empty empty empty

builtinEnvironment :: Mapping
builtinEnvironment = Mapping $ wrapNatives builtins

objectContext :: Object -> Context
objectContext (Module modul) = Context builtinEnvironment empty (defContext modul) empty empty

defContext :: ObjectDef -> Mapping
defContext (ObjectDef defs) = fromList $ map unwrapDef defs

fromList :: [(String,Expression)] -> Mapping
fromList list = Mapping $ M.fromList list

builtins :: (M.Map String Native)
builtins = M.fromList [
    ("+", add),
    ("-", arbre_subtract),
    ("*", mult),
    ("/", divide),
    ("==", eq),
    ("if", arbre_if),
    (">", gt),
    ("<", lt),
    ("and", arbre_and),
    ("or", arbre_or),
    ("not", arbre_not),
    ("+f", float_add),
    ("-f", float_subtract),
    ("*f", float_mult),
    ("/f", float_divide),
    ("==f", float_eq),
    (">f", float_gt)    
  ]

builtinParams :: (M.Map String [String])
builtinParams = M.fromList [
    ("+", ["a", "b"]),
    ("-", ["a", "b"]),
    ("*", ["a", "b"]),
    ("/", ["a", "b"]),
    ("==", ["a", "b"]),
    ("if", ["cond", "then", "else"]),
    (">", ["a", "b"]),
    ("<", ["a", "b"]),
    ("and", ["a", "b"]),
    ("or", ["a", "b"]),
    ("not", ["a"]),
    ("+f", ["a", "b"]),
    ("-f", ["a", "b"]),
    ("*f", ["a", "b"]),
    ("/f", ["a", "b"]),
    ("==f", ["a", "b"]),
    (">f", ["a", "b"])
  ]

functionToMethod :: String -> Def
functionToMethod op = Def op (BlockExp $ Block ["x"] (NativeCall op [(Symref Value ""), (Symref Local "x")]))

functionsToObject :: [String] -> ObjectDef
functionsToObject ops = ObjectDef $ map functionToMethod ops

numdef :: ObjectDef
numdef = functionsToObject ["+", "*", "==", ">"]

floatdef :: ObjectDef
floatdef = functionsToObject ["+f", "-f", "*f", "/f", "==f", ">f"]

booldef :: ObjectDef
booldef = functionsToObject ["and", "or"]

add :: Native
add params@(a:b:[]) = do
    let x = unboxInteger a
    let y = unboxInteger b
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> boxInteger (i+j) numdef
add params = integerError params

arbre_subtract :: Native
arbre_subtract params@(a:b:[]) = do
    let x = unboxInteger a
    let y = unboxInteger b
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> boxInteger (i-j) numdef
arbre_subtract params = integerError params

mult :: Native
mult params@(a:b:[]) = do
    let x = unboxInteger a
    let y = unboxInteger b
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> boxInteger (i*j) numdef
mult params = integerError params

divide :: Native
divide params@(a:b:[]) = do
    let x = unboxInteger a
    let y = unboxInteger b
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> boxInteger (i `div` j) numdef
divide params = integerError params

eq :: Native
eq params@(a:b:[]) = do
    let x = unboxInteger a
    let y = unboxInteger b
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> boxBoolean (i==j) numdef
eq params = integerError params

gt :: Native
gt params@(a:b:[]) = do
    let x = unboxInteger a
    let y = unboxInteger b
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> boxBoolean (i>j) numdef
gt params = integerError params

lt :: Native
lt params@(a:b:[]) = do
    let x = unboxInteger a
    let y = unboxInteger b
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> boxBoolean (i<j) numdef
lt params = integerError params

float_add :: Native
float_add params@(a:b:[]) = do
    let x = unboxFloat a
    let y = unboxFloat b
    case (x,y) of
        (Nothing,_) -> floatError params
        (_,Nothing) -> floatError params
        (Just i, Just j) -> boxFloat (i+j) numdef
float_add params = floatError params

float_subtract :: Native
float_subtract params@(a:b:[]) = do
    let x = unboxFloat a
    let y = unboxFloat b
    case (x,y) of
        (Nothing,_) -> floatError params
        (_,Nothing) -> floatError params
        (Just i, Just j) -> boxFloat (i-j) numdef
float_subtract params = floatError params

float_mult :: Native
float_mult params@(a:b:[]) = do
    let x = unboxFloat a
    let y = unboxFloat b
    case (x,y) of
        (Nothing,_) -> floatError params
        (_,Nothing) -> floatError params
        (Just i, Just j) -> boxFloat (i*j) numdef
float_mult params = floatError params

float_divide :: Native
float_divide params@(a:b:[]) = do
    let x = unboxFloat a
    let y = unboxFloat b
    case (x,y) of
        (Nothing,_) -> floatError params
        (_,Nothing) -> floatError params
        (Just i, Just j) -> boxFloat (i / j) floatdef
float_divide params = floatError params

float_eq :: Native
float_eq params@(a:b:[]) = do
    let x = unboxFloat a
    let y = unboxFloat b
    case (x,y) of
        (Nothing,_) -> floatError params
        (_,Nothing) -> floatError params
        (Just i, Just j) -> boxBoolean (i==j) numdef
float_eq params = floatError params

float_gt :: Native
float_gt params@(a:b:[]) = do
    let x = unboxFloat a
    let y = unboxFloat b
    case (x,y) of
        (Nothing,_) -> floatError params
        (_,Nothing) -> floatError params
        (Just i, Just j) -> boxBoolean (i>j) numdef
float_gt params = floatError params

arbre_if :: Native
arbre_if params@(c:t@(Closure _ _ _ _ _):e@(Closure _ _ _ _ _):[]) = do
    let cond = unboxBoolean c
    case cond of
        Nothing -> booleanError params
        Just True -> Apply t []
        Just False -> Apply e []
arbre_if params = typeError params -- FIXME

arbre_and :: Native
arbre_and params@(a:b:[]) = do
    let a' = unboxBoolean a
    let b' = unboxBoolean b
    case (a',b') of
        (Nothing, _)     -> booleanError params
        (_, Nothing)     -> booleanError params
        (Just i, Just j) -> boxBoolean (i && j) booldef
arbre_and params = typeError params -- FIXME

arbre_or :: Native
arbre_or params@(a:b:[]) = do
    let a' = unboxBoolean a
    let b' = unboxBoolean b
    case (a',b') of
        (Nothing, _)     -> booleanError params
        (_, Nothing)     -> booleanError params
        (Just i, Just j) -> boxBoolean (i || j) booldef
arbre_or params = typeError params -- FIXME

arbre_not :: Native
arbre_not params@(a:[]) = do
    let a' = unboxBoolean a
    case a' of
        Nothing -> booleanError params
        Just i  -> boxBoolean (not i) booldef
arbre_not params = typeError params -- FIXME

wrapNatives :: (M.Map String Native) -> (M.Map String Expression)
wrapNatives natives = M.mapWithKey wrapNative natives

wrapNative :: String -> Native -> Expression
wrapNative key native =
  let maybeParams = M.lookup key builtinParams
  in case maybeParams of
    Just params -> BlockExp $ Block params $ NativeCall key $ map (Symref Local) params
    Nothing     -> Error $ "No params for " ++ key
