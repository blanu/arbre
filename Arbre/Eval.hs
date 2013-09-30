{-# LANGUAGE DeriveDataTypeable #-}

module Arbre.Eval
(
    activate,
    activateMain,
    printObject,
    eval,
    evalMainObjectDef
)
where

import Data.Typeable
import Data.Data
import qualified Data.Map as M

import Arbre.Program
import Arbre.Path
import qualified Arbre.Context as C
import Arbre.Native

data Object = Object (M.Map String Expression)

data ValueChain = ValueChain SourcePath [Value]

data Computation = Computation C.Context Expression deriving (Eq, Show)

data Value =
      ExpressionValue Expression
    | ComputationValue Computation
--    | MonadValue MonadVal
    deriving (Eq, Show)

--data MonadVal =
--      EventMonad Event
--    | MutationMonad Mutation
--    | ReactionMonad Reaction
--    | MetaMondad MonadVal MonadVal
--    deriving (Eq, Show)

--data Event = Event String deriving (Eq, Show, Typeable, Data)
--data Mutation = Mutation String deriving (Eq, Show)
--data Reaction = Reaction [(FunctionName, Block)] deriving (Eq, Show)

activate :: ObjectDef -> Object
activate (ObjectDef defs) = Object $ M.fromList $ map unwrapDef defs

unwrapDef :: Def -> (String, Expression)
unwrapDef (Def name exp) = (name,exp)

activateMain :: Object -> Computation
activateMain (Object lexMap) =
    let context = C.lexChainWithMap C.builtinContext lexMap
        main = C.resolve "main" context
    in Computation context main

printObject :: Object -> String
printObject (Object defMap) = (show defMap)

eval :: Computation -> Expression
eval (Computation context lit@(LiteralExp _)) = lit
eval (Computation context (Symref symref)) =
  C.resolve symref context
eval (Computation context (Apply (Block args exp) params)) =
  let params'     = resolveParams context params
      context'    = apply context args params'
      comp        = Computation context' exp
  in eval comp
eval (Computation context (NativeCall name params)) =
  let params' = resolveParams context params
      maybeFunc = M.lookup name builtins
  in case maybeFunc of
    Just f  -> f params'
    Nothing -> Error $ "Unknown native function " ++ name
eval (Computation context (Call name params)) =
  let params' = resolveParams context params
      maybeFunc = C.resolve name context
  in case maybeFunc of
    BlockExp block -> eval $ Computation C.builtinContext $ Apply block params'
    Error f  -> maybeFunc
    otherwise -> Error $ "Call to a non-function " ++ name

evalToLiteral :: Computation -> Expression
evalToLiteral (Computation context lit@(LiteralExp _)) = lit
evalToLiteral (Computation context error@(Error _)) = error
evalToLiteral comp@(Computation context exp) = evalToLiteral $ Computation context $ eval comp

discardContext :: Computation -> Expression
discardContext (Computation context exp) = exp

resolveParams :: C.Context -> [Expression] -> [Expression]
resolveParams context params =
  let comps = map (Computation context) params
  in map evalToLiteral comps

apply :: C.Context -> [String] -> [Expression] -> C.Context
apply context args params =
  let pairs = zip args params
  in foldl C.addDynamicPair context pairs

evalMainObjectDef :: ObjectDef -> IO()
evalMainObjectDef def = do
  let obj = activate def
  putStrLn $ printObject obj
  let comp = activateMain obj
  putStrLn $ show comp
  let result = evalToLiteral comp
  putStrLn $ show result
