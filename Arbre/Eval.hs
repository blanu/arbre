{-# LANGUAGE DeriveDataTypeable #-}

module Arbre.Eval
(
    activateMain,
    eval,
    evalMainModule,
    evalIterModule,
    evalEventLoopModule,
    evalEventIterModule,
    evalReceiverIterModule,
    evalProgram,
    supereval
)
where

import Data.Typeable
import Data.Data
import qualified Data.Map as M
import Data.Either
import qualified Debug.Trace as T
import qualified Data.Bimap as Bi

import Arbre.Expressions
import Arbre.Path
import Arbre.Native
import Arbre.Print
import Arbre.View
import Arbre.Mutation
import Arbre.Context as C
import Arbre.Event
import Arbre.Receiver

data ValueChain = ValueChain SourcePath [Value]

data Value =
      ExpressionValue Expression
    | ComputationValue Computation
    deriving (Eq, Show)

trace :: String -> a -> a
--trace s a = T.trace s a
trace s a = a

prune :: Computation -> Computation
prune (Computation _ exp) = Computation emptyContext exp

wrap :: Expression -> Computation
wrap exp = Computation emptyContext exp

eval :: Computation -> Computation
eval comp@(Computation context lit@(ObjectExp _)) = wrap lit
eval comp@(Computation context event@(Event _ _)) = wrap event
eval comp@(Computation context comb@(Combine _ _)) = wrap comb
eval comp@(Computation context error@(Error _)) = wrap error
eval comp@(Computation context closure@(Closure _ _ _ _ _)) = wrap closure
eval comp@(Computation context (Receiver rec exp)) = wrap $ Receiver rec (evalToLiteral (Computation context exp))
eval (Computation context (Mutation effect sym value)) = wrap $ Mutation effect sym (evalToLiteral (Computation context value))
eval (Computation context blockExp@(BlockExp _)) = wrap $ C.close context blockExp
eval (Computation context (Symref env symref)) = wrap $ C.resolve env symref context
eval (Computation context app@(Apply block@(BlockExp (Block args exp)) params)) = do
  let eitherParams'     = resolveParams context params
  case eitherParams' of
    Left error -> wrap error
    Right params' -> do
      let context'    = apply context args params'
      let closure     = C.close context' block
      Computation context' closure
eval (Computation context app@(Apply closure@(Closure lex dyn self value (Block args exp)) params)) = do
  let context' = open closure
  let eitherParams'     = resolveParams context' params
  case eitherParams' of
    Left error -> wrap error
    Right params' -> do
      let context''    = apply context' args params'
      Computation context'' exp
eval (Computation context nat@(NativeCall name params)) = do
  let eitherParams' = resolveParams context params
  case eitherParams' of
    Left error -> wrap error
    Right params' -> do
      case params == params' of
        False -> Computation context $ NativeCall name params'
        True  -> do
          let f = applyNative name
          let result = f params'
          let context' = C.stack context
          Computation context' result
eval (Computation context call@(Call (Symref env name) params)) = do
  let eitherParams' = resolveParams context params
  case eitherParams' of
    Left error -> wrap error
    Right params' -> do
      case params == params' of
        False -> Computation context $ Call (Symref env name) params'
        True  -> do
          let maybeFunc = C.resolve env name context
          case maybeFunc of
            BlockExp block -> evalBlock env context params' block
            closure@(Closure _ _ _ _ _) -> evalClosure context closure params' 
            Error f  -> wrap $ Error f
            otherwise -> wrap $ Error $ "Call to a non-function " ++ name ++ ": " ++ show maybeFunc
eval (Computation context exp) = wrap $ Error $ "unimplemented eval: " ++ show exp

evalBlock :: Environment -> Context -> [Expression] -> Block -> Computation
evalBlock env (Context lex dyn self value local) params block = do
    let context' = Context lex dyn self value local
    let closure = C.close context' (BlockExp block)
    let expr = Apply closure params
    Computation context' expr

evalClosure :: Context -> Expression -> [Expression] -> Computation
evalClosure context closure@(Closure _ _ _ _ _) params = do
    Computation context $ Apply closure params

evalToLiteral :: Computation -> Expression
evalToLiteral (Computation context lit@(ObjectExp _)) = lit
evalToLiteral (Computation context error@(Error _)) = error
evalToLiteral (Computation context (Receiver rec exp)) = Receiver rec (evalToLiteral (Computation context exp))
evalToLiteral (Computation context (Mutation effect sym value)) = Mutation effect sym (evalToLiteral (Computation context value))
evalToLiteral (Computation context (Event event value)) = Event event $ evalToLiteral $ Computation context value
evalToLiteral (Computation context (Combine a b)) = Combine (evalToLiteral $ Computation context a) (evalToLiteral $ Computation context b)
evalToLiteral (Computation context closure@(Closure _ _ _ _ _)) = closure
evalToLiteral (Computation context block@(BlockExp _)) = C.close context block
evalToLiteral comp@(Computation context sym@(Symref env symref)) =
  let comp' = eval comp
      exp'  = discardContext comp'
      ss    = printExpression (Views []) "" sym
      cs    = printExpression (Views []) "" exp'
--  in trace ("etl symref " ++ ss ++ ": " ++ cs) exp'
  in exp'
evalToLiteral comp@(Computation context exp@(Call _ _)) = trace (printExpression (Views []) "" exp) $ evalToLiteral $ eval comp
evalToLiteral comp@(Computation context exp@(NativeCall _ _)) = trace (printExpression (Views []) "" exp) $ evalToLiteral $ eval comp
evalToLiteral comp@(Computation context exp) = evalToLiteral $ eval comp

evalStepToLiteral :: Computation -> Expression
evalStepToLiteral comp@(Computation context exp) =
    let result = evalToLiteral comp
    in case result of
        lit@(ObjectExp _)    -> lit
        error@(Error _)      -> error
        mut@(Mutation _ _ _) ->
          let context' = applyMutation mut context
          in evalStepToLiteral $ Computation context' exp
        otherwise            -> Error $ "step returned unsupported type: " ++ show result

evalEventLoop :: Computation -> IO()
evalEventLoop comp@(Computation context exp) =
    let result = evalToLiteral comp
    in case result of
        error@(Error _)      -> putStrLn $ show error
        Event event value -> do
          applyEvent event value
          evalEventLoop comp
        otherwise            -> putStrLn $ show $ Error $ "event loop returned unsupported type: " ++ show result

evalEventStep :: Computation -> IO()
evalEventStep comp@(Computation context exp) =
    let result = evalToLiteral comp
    in case result of
        error@(Error _)      -> putStrLn $ show error
        ev@(Event event value) -> do
          applyEvent event value
          T.trace (printExpression (Views []) "" ev) $ evalEventStep comp
        mut@(Mutation _ _ _) -> do
          let context' = applyMutation mut context
          T.trace (printExpression (Views []) "" mut) $ evalEventStep $ Computation context' exp
        comb@(Combine _ _) -> do
          context' <- applyCombine comb context
          T.trace (printExpression (Views []) "" comb) $ evalEventStep $ Computation context' exp
        otherwise            -> putStrLn $ show $ Error $ "event step returned unsupported type: " ++ show result

applyCombine :: Expression -> Context -> IO Context
applyCombine (Combine (Event event value) mut@(Mutation _ _ _)) context = do
  applyEvent event value
  return $ applyMutation mut context
applyCombine (Combine mut@(Mutation _ _ _) event@(Event _ _)) context = applyCombine (Combine event mut) context
applyCombine (Combine (Event event value) (Event event2 value2)) context = do
  applyEvent event value
  applyEvent event2 value2
  return context
applyCombine (Combine mut@(Mutation _ _ _) mut2@(Mutation _ _ _)) context = do
  let context' = applyMutation mut context
  return $ applyMutation mut2 context'
applyCombine (Combine (Event event value) comb@(Combine _ _)) context = do
  applyEvent event value
  applyCombine comb context
applyCombine (Combine comb@(Combine _ _) (Event event value)) context = do
  context' <- applyCombine comb context
  applyEvent event value
  return $ context'
applyCombine (Combine mut@(Mutation _ _ _) comb@(Combine _ _)) context = do
  let context' = applyMutation mut context
  applyCombine comb context'
applyCombine (Combine comb@(Combine _ _) mut@(Mutation _ _ _)) context = do
  context' <- applyCombine comb context
  return $ applyMutation mut context
applyCombine (Combine comb@(Combine _ _) comb2@(Combine _ _)) context = do
  context' <- applyCombine comb context
  applyCombine comb context'

discardContext :: Computation -> Expression
discardContext (Computation context exp) = exp

resolveParams :: Context -> [Expression] -> Either Expression [Expression]
resolveParams context params = do
  let comps = map (Computation context) params
  let params' = map evalToLiteral comps
  checkParams params' params'

checkParams :: [Expression] -> [Expression] -> Either Expression [Expression]
checkParams [] original = Right original
checkParams (error@(Error message):params) original = Left error
checkParams (_:params) original = checkParams params original  

apply :: Context -> [String] -> [Expression] -> Context
apply context args params =
  let pairs = zip args params
      context' = C.stack context
      context'' = foldl (C.bindPair Local) context' pairs
--  in trace ("apply: " ++ printPairs pairs) context'' 
  in context'' 

evalMainModule :: Expression -> IO()
evalMainModule (ObjectExp modul) = do
  putStrLn $ "module: \n" ++ printObjectFull (Views []) "" modul
  let comp@(Computation _ cexp) = activateMain modul
  putStrLn $ "comp: " ++ printExpression (Views []) "" cexp
  let result = evalToLiteral comp
  putStrLn $ "result: " ++ printExpression (Views []) "" result
  putStrLn ""
evalMainModule _ = return ()

activateMain :: Object -> Computation
activateMain modul = do
    let context = objectContext modul
    let main = C.resolve Self "main" context
    Computation context main

evalIterModule :: Expression -> IO()
evalIterModule (ObjectExp modul) = do
  putStrLn $ "module: \n" ++ printObjectFull (Views []) "" modul
  let start@(Computation _ cexp) = activateIterStart modul
  putStrLn $ "start: " ++ printExpression (Views []) "" cexp
  let definitions@(Mutation _ _ _) = evalToLiteral start
  putStrLn $ "definitions: " ++ printExpression (Views []) "" definitions

  let (Computation context stepExp) = activateIterStep modul
  let stepContext@(Context _ dyn _ _ _) = applyMutation definitions context
  putStrLn $ "stepContext: " ++ printMapping Dyn dyn
  putStrLn $ "stepExp: " ++ printExpression (Views []) "" stepExp

  let stepComp = Computation stepContext stepExp
  let result = evalStepToLiteral stepComp
  putStrLn $ "result: " ++ printExpression (Views []) "" result
  putStrLn ""
evalIterModule _ = return ()

activateIterStart :: Object -> Computation
activateIterStart modul = do
    let context = objectContext modul
    let main = C.resolve Self "start" context
    Computation context main

activateIterStep :: Object -> Computation
activateIterStep modul = do
    let context = objectContext modul
    let main = C.resolve Self "step" context
    Computation context main

evalEventLoopModule :: Expression -> IO()
evalEventLoopModule (ObjectExp modul) = do
  putStrLn $ "module: \n" ++ printObjectFull (Views []) "" modul
  let comp@(Computation _ cexp) = activateEventLoop modul
  putStrLn $ "comp: " ++ printExpression (Views []) "" cexp
  evalEventLoop comp
evalEventLoopModule _ = return ()

evalEventIterModule :: Expression -> IO()
evalEventIterModule (ObjectExp modul) = do
  putStrLn $ "module: \n" ++ printObjectFull (Views []) "" modul
  let start@(Computation _ cexp) = activateIterStart modul
  putStrLn $ "start: " ++ printExpression (Views []) "" cexp
  let definitions = evalToLiteral start
  putStrLn $ "definitions: " ++ printExpression (Views []) "" definitions
  let (Computation context stepExp) = activateIterStep modul

  case definitions of
    defs@(Mutation _ _ _) -> evalStep (applyMutation definitions context) stepExp
    defs@(Combine _ _)    -> do
      context' <- applyCombine definitions context
      evalStep context' stepExp
evalEventIterModule _ = return ()

evalStep :: Context -> Expression -> IO()
evalStep stepContext@(Context _ dyn _ _ _) stepExp = do
  putStrLn $ "stepContext: " ++ printMapping Dyn dyn
  putStrLn $ "stepExp: " ++ printExpression (Views []) "" stepExp

  let stepComp = Computation stepContext stepExp
  evalEventStep stepComp

activateEventLoop :: Object -> Computation
activateEventLoop modul = do
    let context = objectContext modul
    let main = C.resolve Self "loop" context
    Computation context main

activateNamed :: String -> Object -> Computation
activateNamed name modul = do
    let context = objectContext modul
    let main = C.resolve Self name context
    Computation context main

evalReceiverIterModule :: Expression -> IO()
evalReceiverIterModule (ObjectExp modul) = do
  putStrLn $ "module: \n" ++ printObjectFull (Views []) "" modul
  let start@(Computation _ cexp) = activateIterStart modul
  putStrLn $ "start: " ++ printExpression (Views []) "" cexp
  let definitions = evalToLiteral start
  putStrLn $ "definitions: " ++ printExpression (Views []) "" definitions
  let (Computation context stepExp) = activateMain modul

  case definitions of
    defs@(Mutation _ _ _) -> evalReceiverStep $ Computation (applyMutation definitions context) stepExp
    defs@(Combine _ _)    -> do
      context' <- applyCombine definitions context
      evalReceiverStep $ Computation context' stepExp
    otherwise -> putStrLn $ "Unsupported receiver definitions " ++ show definitions
evalReceiverEventModule _ = return ()

evalReceiverStep :: Computation -> IO()
evalReceiverStep comp@(Computation context exp) = do
    let result = evalToLiteral comp
    case result of
        obj@(ObjectExp _) -> do
          putStrLn $ "Got result: " ++ printExpression (Views[]) "" obj
        rec@(Receiver receiver rexp) -> do
          comp'@(Computation context' rexp) <- applyReceiver receiver rexp context
          let exp = evalToLiteral comp'
          evalReceiverStep $ Computation context' exp
        (Event event value) -> do
          let value' = evalToLiteral $ Computation context value
          applyEvent event value'
        (Combine event@(Event _ _) receiver@(Receiver _ _)) -> do
          evalReceiverStep $ Computation context event
          evalReceiverStep $ Computation context receiver
        (Error error) -> putStrLn $ "Error: " ++ error
        otherwise            -> putStrLn $ show $ Error $ "er event step returned unsupported type: " ++ show result

receiverEvalToLiteral :: Computation -> IO Expression
receiverEvalToLiteral (Computation context (Receiver receiver rexp)) = do
  comp@(Computation rcontext rvalue) <- applyReceiver receiver rexp context
  let value = evalToLiteral comp
  case value of
    rec@(Receiver _ _) -> receiverEvalToLiteral $ Computation context value
    otherwise          -> return value

evalProgram :: Expression -> IO()
evalProgram (ObjectExp modul) = do
  let (Computation context stepExp) = activateMain modul

  case stepExp of
    (ProgramExp program) -> applyProgram context program
    otherwise -> putStrLn $ "Unsupported expression for evalProgram " ++ show stepExp
evalProgram _ = return ()

applyProgram :: Context -> Program -> IO()
applyProgram context (PrintVal value) = do
  putStrLn $ printExpression (Views []) "" $ evalToLiteral $ Computation context value
applyProgram context (Emit exp) = do
  let exp' = evalToLiteral $ Computation context exp
  case exp' of
    (Event event value) -> applyEvent event $ evalToLiteral $ Computation context value
    otherwise           -> putStrLn $ "Type error, can't emit non-events: " ++ show exp'
applyProgram context (Sequence first second) = do
  applyProgram context first
  applyProgram context second
applyProgram context (Receive first second) = do
  case first of
    (Receiver receiver value) -> do
      comp <- applyReceiver receiver (evalToLiteral $ Computation context value) context
      return ()
    otherwise           -> putStrLn $ "Type error, can't receive non-receivers: " ++ show first
applyProgram context (Iterate exp prog) = do
  case exp of
    (Receiver receiver value) -> do
      (Computation context' exp') <- applyReceiver receiver (evalToLiteral $ Computation context value) context
      applyProgram context' (Iterate exp' prog)
    otherwise           -> return ()

supereval :: Expression -> IO()
supereval (ObjectExp modul) = do
  let comp@(Computation context stepExp) = activateMain modul
  let exp' = evalToLiteral comp

  superevalLoop $ Computation context exp'

superevalLoop :: Computation -> IO()
superevalLoop comp = do
  putStrLn "---------------------------------------"
  putStrLn $ "superevalLoop: " ++ printComputation comp
--  putStrLn $ "superevalLoop: " ++ show comp
  maybeComp <- superevalIter comp
  case maybeComp of
    Just comp' -> superevalLoop comp'
    Nothing    -> return ()

superevalIter :: Computation -> IO(Maybe Computation)
superevalIter comp@(Computation context exp) = do
  let comp'@(Computation context' exp') = Computation context $ evalToLiteral comp
  case exp' of
    (Event event value) -> do
      applyEvent event $ evalToLiteral $ Computation context' value
      return Nothing
    (Combine first second) -> do
      val <- superevalIter $ Computation context' first
      return $ Just $ Computation context' second
    (Receiver receiver value) -> do
      comp'' <- applyReceiver receiver value context'
      return $ Just comp''
    otherwise -> return $ Just $ comp'

