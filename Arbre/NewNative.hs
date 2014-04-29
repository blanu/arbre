{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances #-}

module Arbre.NewNative
(
  builtins,
  wrapNatives,
  numdef,
  booldef,
  stringdef,
  builtinContext,
  builtinEnvironment,
  objectContext
)
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bi
import Data.Typeable
import Data.Data
import Debug.Trace

import Arbre.Program
import Arbre.Box
import Arbre.Context
import Arbre.Print
import Arbre.View

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
  | Not

applyNative :: NativeType -> Native
applyNative IntegerAdd = add
applyNative IntegerSubtract = arbre_subtract
applyNative IntegerMultiply = mult
applyNative IntegerDivide = divide
applyNative IntegerEquals = eq
applyNative IntegerGreaterThan = gt
applyNative IntegerLessThan = lt
applyNative FloatAdd = float_add
applyNative FloatSubtract = float_subtract
applyNative FloatMultiply = float_mult
applyNative FloatDivide = float_divide
applyNative FloatEquals = float_eq
applyNative FloatGreaterThan = float_gt
--applyNative FloatLessThan = float_lt
applyNative StringEquals = string_eq
applyNative Append = string_append
--applyNative BooleanEquals = boolean_eq
applyNative If = arbre_if
applyNative And = arbre_and
applyNative Or = arbre_or
applyNative Not = arbre_not

builtins :: (M.Map String NativeType)
builtins = M.fromList [
    ("+", IntegerAdd),
    ("-", IntegerSubtract),
    ("*", IntegerMultiply),
    ("/", IntegerDivide),
    ("==", IntegerEquals),
    ("if", If),
    (">", IntegerGreaterThan),
    ("<", IntegerLessThan),
    ("and", And),
    ("or", Or),
    ("not", Not),
    ("+f", FloatAdd),
    ("-f", FloatSubtract),
    ("*f", FloatMultiply),
    ("/f", FloatDivide),
    ("==f", FloatEquals),
    (">f", FloatGreaterThan),
    ("append", Append),
    ("==s", StringEquals)
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
    (">f", ["a", "b"]),
    ("append", ["a", "b"]),
    ("==s", ["a", "b"])
  ]

functionToMethod :: String -> Def
functionToMethod name = do
  let maybeOp = Bi.lookup name builtins
  case maybeOp of
    Just op -> Def name (BlockExp $ Block ["x"] (NativeCall op [(Symref Value ""), (Symref Local "x")]))

functionsToObject :: [String] -> ObjectDef
functionsToObject ops = ObjectDef $ map functionToMethod ops

numdef :: ObjectDef
numdef = functionsToObject ["+", "*", "==", ">"]

floatdef :: ObjectDef
floatdef = functionsToObject ["+f", "-f", "*f", "/f", "==f", ">f"]

booldef :: ObjectDef
booldef = functionsToObject ["and", "or"]

stringdef :: ObjectDef
stringdef = functionsToObject ["append", "==s"]

add :: Native
add params@(a:b:[]) = do
    let x = unbox a :: Maybe Integer
    let y = unbox b :: Maybe Integer
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> box (i+j)
add params = integerError params

arbre_subtract :: Native
arbre_subtract params@(a:b:[]) = do
    let x = unbox a :: Maybe Integer
    let y = unbox b :: Maybe Integer
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> box (i-j)
arbre_subtract params = integerError params

mult :: Native
mult params@(a:b:[]) = do
    let x = unbox a :: Maybe Integer
    let y = unbox b :: Maybe Integer
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> box (i*j)
mult params = integerError params

divide :: Native
divide params@(a:b:[]) = do
    let x = unbox a :: Maybe Integer
    let y = unbox b :: Maybe Integer
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> box (i `div` j)
divide params = integerError params

eq :: Native
eq params@(a:b:[]) = do
    let x = unbox a :: Maybe Integer
    let y = unbox b :: Maybe Integer
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> box (i==j)
eq params = integerError params

gt :: Native
gt params@(a:b:[]) = do
    let x = unbox a :: Maybe Integer
    let y = unbox b :: Maybe Integer
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> box (i>j)
gt params = integerError params

lt :: Native
lt params@(a:b:[]) = do
    let x = unbox a :: Maybe Integer
    let y = unbox b :: Maybe Integer
    case (x,y) of
        (Nothing,_) -> integerError params
        (_,Nothing) -> integerError params
        (Just i, Just j) -> box (i<j)
lt params = integerError params

float_add :: Native
float_add params@(a:b:[]) = do
    let x = unbox a :: Maybe Float
    let y = unbox b :: Maybe Float
    case (x,y) of
        (Nothing,_) -> floatError params
        (_,Nothing) -> floatError params
        (Just i, Just j) -> box (i+j)
float_add params = floatError params

float_subtract :: Native
float_subtract params@(a:b:[]) = do
    let x = unbox a :: Maybe Float
    let y = unbox b :: Maybe Float
    case (x,y) of
        (Nothing,_) -> floatError params
        (_,Nothing) -> floatError params
        (Just i, Just j) -> box (i-j)
float_subtract params = floatError params

float_mult :: Native
float_mult params@(a:b:[]) = do
    let x = unbox a :: Maybe Float
    let y = unbox b :: Maybe Float
    case (x,y) of
        (Nothing,_) -> floatError params
        (_,Nothing) -> floatError params
        (Just i, Just j) -> box (i*j)
float_mult params = floatError params

float_divide :: Native
float_divide params@(a:b:[]) = do
    let x = unbox a :: Maybe Float
    let y = unbox b :: Maybe Float
    case (x,y) of
        (Nothing,_) -> floatError params
        (_,Nothing) -> floatError params
        (Just i, Just j) -> box (i / j)
float_divide params = floatError params

float_eq :: Native
float_eq params@(a:b:[]) = do
    let x = unbox a :: Maybe Float
    let y = unbox b :: Maybe Float
    case (x,y) of
        (Nothing,_) -> floatError params
        (_,Nothing) -> floatError params
        (Just i, Just j) -> box (i==j)
float_eq params = floatError params

float_gt :: Native
float_gt params@(a:b:[]) = do
    let x = unbox a :: Maybe Float
    let y = unbox b :: Maybe Float
    case (x,y) of
        (Nothing,_) -> floatError params
        (_,Nothing) -> floatError params
        (Just i, Just j) -> box (i>j)
float_gt params = floatError params

arbre_if :: Native
arbre_if params@(c:t@(Closure _ _ _ _ _):e@(Closure _ _ _ _ _):[]) = do
    let cond = unbox c :: Maybe Bool
    case cond of
        Nothing -> booleanError params
        Just True  -> Apply t []
        Just False -> Apply e []
arbre_if params = typeError params -- FIXME

arbre_and :: Native
arbre_and params@(a:b:[]) = do
    let a' = unbox a :: Maybe Bool
    let b' = unbox b :: Maybe Bool
    case (a',b') of
        (Nothing, _)     -> booleanError params
        (_, Nothing)     -> booleanError params
        (Just i, Just j) -> box (i && j)
arbre_and params = typeError params -- FIXME

arbre_or :: Native
arbre_or params@(a:b:[]) = do
    let a' = unbox a :: Maybe Bool
    let b' = unbox b :: Maybe Bool
    case (a',b') of
        (Nothing, _)     -> booleanError params
        (_, Nothing)     -> booleanError params
        (Just i, Just j) -> box (i || j)
arbre_or params = typeError params -- FIXME

arbre_not :: Native
arbre_not params@(a:[]) = do
    let a' = unbox a :: Maybe Bool
    case a' of
        Nothing -> booleanError params
        Just i  -> box (not i)
arbre_not params = typeError params -- FIXME

string_append :: Native
string_append params@(a:b:[]) = do
    let a' = unbox a :: Maybe String
    let b' = unbox b :: Maybe String
    case (a',b') of
        (Nothing, _)     -> stringError params
        (_, Nothing)     -> stringError params
        (Just i, Just j) -> box (i ++ j)
string_append params = typeError params -- FIXME

string_eq :: Native
string_eq params@(a:b:[]) = do
    let x = unbox a :: Maybe String
    let y = unbox b :: Maybe String
    case (x,y) of
        (Nothing,_) -> stringError params
        (_,Nothing) -> stringError params
        (Just i, Just j) -> box (i==j)
string_eq params = stringError params

wrapNatives :: (M.Map String NativeType) -> (M.Map String Expression)
wrapNatives natives = M.mapWithKey wrapNative natives

wrapNative :: String -> Native -> Expression
wrapNative key native =
  let maybeParams = M.lookup key builtinParams
  in case maybeParams of
    Just params -> BlockExp $ Block params $ NativeCall key $ map (Symref Local) params
    Nothing     -> Error $ "No params for " ++ key

class Boxable a where
  box :: a -> Expression
  unbox :: Expression -> Maybe a

instance Boxable String where
  box a = ObjectExp $ Object (LiteralState $ StringLit a) stringdef
  unbox exp =
    case exp of
      (ObjectExp (Object (LiteralState (StringLit a)) _)) -> Just a
      otherwise -> Nothing

instance Boxable Integer where
  box a = ObjectExp $ Object (LiteralState $ IntegerLit a) numdef
  unbox exp =
    case exp of
      (ObjectExp (Object (LiteralState (IntegerLit a)) _)) -> Just a
      otherwise -> Nothing

instance Boxable Bool where
  box a = ObjectExp $ Object (LiteralState $ BooleanLit a) booldef
  unbox exp =
    case exp of
      (ObjectExp (Object (LiteralState (BooleanLit a)) _)) -> Just a
      otherwise -> Nothing

instance Boxable Float where
  box a = ObjectExp $ Object (LiteralState $ FloatLit a) floatdef
  unbox exp =
    case exp of
      (ObjectExp (Object (LiteralState (FloatLit a)) _)) -> Just a
      otherwise -> Nothing

boxString :: String -> Expression
boxString s = box s

unboxString :: Expression -> Maybe String
unboxString exp = unbox exp :: Maybe String

integerError :: [Expression] -> Expression
integerError params = Error $ "Type error, not integer literals" ++ (show params)

floatError :: [Expression] -> Expression
floatError params = Error $ "Type error, not float literals" ++ (show params)

unboxFloat :: Expression -> Maybe Float
unboxFloat (ObjectExp (Object (LiteralState (FloatLit i)) _)) = Just i
unboxFloat _ = Nothing

booleanError :: [Expression] -> Expression
booleanError params = Error $ "Type error, not boolean literals" ++ (show params)

stringError :: [Expression] -> Expression
stringError params = Error $ "Type error, not string literals" ++ (show params)

typeError :: [Expression] -> Expression
typeError params = Error $ "General type error: " ++ (show params)
