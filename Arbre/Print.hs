{-# LANGUAGE DeriveDataTypeable #-}

module Arbre.Print
(
    printViews,
    printProg,
    printExpression,
    printObjectFull,
    printArgs,
    printComputation,
    printComputationContext,
    printContext,
    printPairs,
    printMapping,
    printClosure,
    printClosureContext
)
where

import qualified Data.Map as M
import qualified Data.List as L

import Arbre.Expressions
import Arbre.Box
import Arbre.View
import Arbre.Context
import Arbre.Native

printPairs :: [(String,Expression)] -> String
printPairs pairs = "(" ++ (L.intercalate "," $ map (printFullBinding "") pairs) ++ ")"

printComputation :: Computation -> String
printComputation (Computation context exp) = "<<... | " ++ printExpression (Views []) "" exp

printComputationContext :: Computation -> String
printComputationContext (Computation context exp) = printContext context

printObjectFull :: Views -> String -> Object -> String
printObjectFull views ind (Module defMap) = printObjectDef views defMap ++ "\n"
printObjectFull views ind (Object state defMap) = show state ++ ": " ++ printObjectDef views defMap

printViews :: Views -> IO()
printViews views = putStrLn $ show views

printProg :: Views -> ObjectDef -> IO()
printProg views (ObjectDef defs) = putStrLn $ printDefs views defs

printObjectDef :: Views -> ObjectDef -> String
printObjectDef views (ObjectDef defs) = printDefs views defs

printDefs :: Views -> [Def] -> String
printDefs views [] = ""
printDefs views defs = L.intercalate "\n" $ map (printDef views) defs

printDef :: Views -> Def -> String
printDef views (Def name defExpr) =
    name ++ ": " ++ (printExpression views "" defExpr)

printEnvironment :: Environment -> String
printEnvironment Lex = "^"
printEnvironment Dyn = "$"
printEnvironment Self = "~"
printEnvironment Value = "&"
printEnvironment Local = ":"

printParams :: [String] -> String
printParams params = L.intercalate ", " params

printBlock :: Views -> String -> Expression -> String
printBlock views ind expr = ind ++ printExpression views ind expr

printExpression :: Views -> String -> Expression -> String
printExpression views ind (ObjectExp obj) = printObject obj
printExpression views ind (BlockExp block) = printNestedBlock views ind block
printExpression views ind (Symdef symdef) = printEnvironment Dyn ++ symdef
printExpression views ind symref@(Symref env sym) = printEnvironment env ++ sym
printExpression views ind call@(Call _ _) = printCall views ind call
printExpression views ind (Apply clos@(Closure _ _ _ _ _) params) =
  printClosure views ind clos ++ " . (" ++ printArgs views ind params ++ ")"
printExpression views ind clos@(Closure _ _ _ _ _) = printClosure views ind clos
printExpression views ind call@(NativeCall _ _) = printNativeCall views ind call  
printExpression views ind (Error message) = "<error: " ++ message ++ ">"
printExpression views ind (Mutation Define sym value) = ind ++ (printExpression views ind sym) ++ " := " ++ printExpression views ind value
printExpression views ind (Mutation Set sym value) = ind ++ (printExpression views ind sym) ++ " = " ++ printExpression views ind value
printExpression views ind (Event Print value) = ind ++ "io <- " ++ printExpression views ind value
printExpression views ind (Receiver Stdin block@(BlockExp (Block args exp))) = ind ++ "io -> " ++ printParams args ++ " do " ++ printExpression views ind block
printExpression views ind (Receiver Stdin clos@(Closure _ _ _ _ _)) = ind ++ "io -> " ++ printClosure views ind clos
printExpression views ind (Combine a b) = ind ++ (printExpression views ind a) ++ "; " ++ printExpression views ind b
printExpression views ind exp = "<print not implemented>" ++ show exp

printClosure :: Views -> String -> Expression -> String
printClosure views ind (Closure lex dyn self value block) =
  "{...|" ++ printNestedBlock views ind block ++ "}"

printClosureContext :: Views -> String -> Expression -> String
printClosureContext views ind (Closure lex dyn self value block) =
  "{" ++ printMapping Lex lex ++ ", " ++ printMapping Dyn dyn ++ ", " ++ printMapping Self self ++ "," ++ printMapping Value value ++ "|" ++ printNestedBlock views ind block ++ "}"

printContext :: Context -> String
printContext (Context lex dyn self value local) =
  "{" ++ printMapping Lex lex ++ ", " ++ printMapping Dyn dyn ++ ", " ++ printMapping Self self ++ "," ++ printMapping Value value ++ "," ++ printMapping Local local ++ "}"

printMapping :: Environment -> Mapping -> String
printMapping env (Mapping mapping) =
  if length (M.toList mapping) == 0
    then printEnvironment env ++ "None"
    else L.intercalate "," $ map (printBinding (printEnvironment env)) (M.toList mapping)

printBinding :: String -> (String, Expression) -> String
--printBinding prefix (name, value) = prefix ++ name ++ ": " ++ printExpression (Views []) "" value
printBinding prefix (name, value) = prefix ++ name

printFullBinding :: String -> (String, Expression) -> String
printFullBinding prefix (name, value) = prefix ++ name ++ ": " ++ printExpression (Views []) "" value

printCall :: Views -> String -> Expression -> String
printCall views ind call@(Call (Symref env name) args) =
    case (getView views name) of
        Just view -> printCallWithView views ind view call
        Nothing -> name ++ "(" ++ (printArgs views ind args) ++ ")"
printCall views ind exp = "<unimplemented call> " ++ show exp

printCallWithView :: Views -> String -> View -> Expression -> String
printCallWithView views ind (View name []) call@(Call _ args) = ""
printCallWithView views ind (View name (part:parts)) call@(Call _ args) =
    printViewPartWithCall views ind part args ++ " " ++ printCallWithView views ind (View name parts) call

printNativeCall :: Views -> String -> Expression -> String
printNativeCall views ind call@(NativeCall native args) = do
    let name = nativeName native
    case (getView views name) of
        Just view -> printCallWithView views ind view call
        Nothing -> "#" ++ name ++ "(" ++ (printArgs views ind args) ++ ")"
printNativeCall views ind exp = "<unimplemented native call> " ++ show exp

printViewPartWithCall :: Views -> String -> ViewPart -> [Expression] -> String
printViewPartWithCall views ind (SymrefView index) args =
    let arg = args !! index
    in printArg views ind arg
printViewPartWithCall views ind (SymdefView index) args =
    let arg = args !! index
    in printArg views ind arg
--printViewPartWithCall views ind (BlockView index) args =
--    let arg = args !! index
--    in printIndentedBlockArg views (ind ++ "  ") arg
printViewPartWithCall views ind (FuncView str) args = str
printViewPartWithCall views ind (SugarView str) args = str

--printIndentedBlockArg :: Views -> String -> Expression -> String
--printIndentedBlockArg views ind arg =
--    case arg of
--        BlockExp block ->
--            printIndentedBlock views ind block
--        otherwise ->
--            "Type Error: Block required for view"

printIndentedBlock :: Views -> String -> Block -> String
printIndentedBlock views ind (Block _ block) =
    "\n" ++ ind ++ printBlock views ind block ++ "\n"

printArgs :: Views -> String -> [Expression] -> String
printArgs views ind args = L.intercalate ", " (map (printArg views ind) args)

printArg :: Views -> String -> Expression -> String
printArg views ind arg = printExpression views ind arg

printObject :: Object -> String
printObject (Module mod) = "<module>"
printObject (Object state def) = "@" ++ printState state

printState :: State -> String
printState (ObjectState obj) =  printObject obj
printState (LiteralState lit) = printLiteral lit

printLiteral :: Literal -> String
printLiteral (StringLit literal) = show literal
printLiteral (IntegerLit literal) = show literal
printLiteral (BooleanLit True) = "true"
printLiteral (BooleanLit False) = "false"
printLiteral (FloatLit literal) = show literal
printLiteral (ListLit literal) = "list"
printLiteral (MapLit literal) = "map"

--printSymdef :: Symdef -> String
--printSymdef (Symdef symdef) = symdef

printNestedBlock :: Views -> String -> Block -> String
printNestedBlock views ind (Block allparams@(param:params) expr) = "{with " ++ (printParams allparams) ++ ": " ++ (printExpression views ind expr) ++ "}"
printNestedBlock views ind (Block [] expr) = "{" ++ printExpression views ind expr ++ "}"

--data LiveBlock = LiveBlock LexicalContext DynamicContext Block deriving (Eq, Show, Typeable, Data)

--data Value =
--      LiteralValue Literal
--    | ComputationValue Computation
--    | MonadValue Monad
--    | ProgramValue Object
--    deriving (Eq, Show, Typeable, Data)

--data Computation = Computation LiveBlock
--data Monad =
--      EventMonad [Event]
--    | MutationMonad [Mutation]
--    | ComboMonad EventMonad MutationMonad
--    deriving (Eq, Show, Typeable, Data)

--data Event = Event String deriving (Eq, Show, Typeable, Data)
--data Mutation = Mutation String deriving ()

--load :: Object -> LiveBlock

--eval :: LiveBlock -> Value

