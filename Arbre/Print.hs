{-# LANGUAGE DeriveDataTypeable #-}

module Arbre.Print
(
    printViews,
    printProg
)
where

import qualified Data.Map as M
import qualified Data.List as L

import Arbre.Program
import Arbre.Box
import Arbre.View

printViews :: Views -> IO()
printViews views = putStrLn $ show views

printProg :: Views -> ObjectDef -> IO()
printProg views (ObjectDef defs) = putStrLn $ printDefs views defs

printDefs :: Views -> [Def] -> String
printDefs views [] = ""
printDefs views (def:defs) = printDef views def ++ printDefs views defs

printDef :: Views -> Def -> String
printDef views (Def name defExpr) =
    name ++ (printDefExpression views "" defExpr) ++ "\n\n"

printDefExpression :: Views -> String -> Expression -> String
printDefExpression views ind (ObjectExp object) =
    ":" ++ "\n" ++ (printObject object)
--printDefExpression views ind (BlockExpression params block) =
--    "(" ++ (printParams params) ++ "):" ++ "\n" ++ (printBlock views "  " block)

printParams :: [String] -> String
printParams params = L.intercalate ", " params

printBlock :: Views -> String -> Expression -> String
printBlock views ind expr = ind ++ printExpression views ind expr

printExpression :: Views -> String -> Expression -> String
printExpression views ind (ObjectExp obj) = printObject obj
--printExpression views ind (BlockExpression block) = printNestedBlock views ind block
--printExpression views ind (SymdefExpression symdef) = printSymdef symdef
printExpression views ind symref@(Symref _) = printSymref symref
printExpression views ind call@(Call _ _) = printCall views ind call

printCall :: Views -> String -> Expression -> String
printCall views ind call@(Call name args) =
    case (getView views name) of
        Just view -> printCallWithView views ind view call
        Nothing -> name ++ "(" ++ (printArgs views ind args) ++ ")"

printCallWithView :: Views -> String -> View -> Expression -> String
printCallWithView views ind (View name []) call@(Call _ args) = ""
printCallWithView views ind (View name (part:parts)) call@(Call _ args) =
    printViewPartWithCall views ind part args ++ " " ++ printCallWithView views ind (View name parts) call

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
printObject (Object state def) = "<object: " ++ printState state ++ ">"

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

printSymref :: Expression -> String
printSymref (Symref symref) = symref

printNestedBlock :: Views -> String -> Block -> String
printNestedBlock views ind (Block allparams@(param:params) expr) = "{" ++ (printParams allparams) ++ ": " ++ (printExpression views ind expr) ++ "}"
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

