{-# LANGUAGE DeriveDataTypeable #-}

module Arbre.Core
(
    printProg
)
where

import qualified Data.Map as M
import qualified Data.List as L

import Arbre.Program

printProg :: LiteralProgram -> IO()
printProg (LiteralProgram defs) = putStrLn $ printDefs defs

printDefs :: [Def] -> String
printDefs [] = ""
printDefs (def:defs) = printDef def ++ printDefs defs

printDef :: Def -> String
printDef (Def (FunctionName name) (Block params expr)) =
    name ++ "(" ++ (printParams params) ++ "):" ++ "\n" ++ (printBlock "  " expr) ++ "\n\n"

printParams :: [Param] -> String
printParams params = L.intercalate ", " (map unwrapParam params)

unwrapParam :: Param -> String
unwrapParam (Param param) = param

printBlock :: String -> Expression -> String
printBlock ind expr = ind ++ printExpression ind expr

printExpression :: String -> Expression -> String
printExpression ind (LiteralExpression literal) = printLiteral literal
printExpression ind (SymrefExpression symref) = printSymref symref
printExpression ind (FunctionExpression func) = printCall ind func
printExpression ind (BlockExpression block) = printNestedBlock ind block
printExpression ind (SymdefExpression symdef) = printSymdef symdef

printCall :: String -> FunctionCall -> String
printCall ind (FunctionCall (FunctionName name) args) = name ++ "(" ++ (printArgs ind args) ++ ")"

printArgs :: String -> [Arg] -> String
printArgs ind args = L.intercalate ", " (map (printArg ind) args)

printArg :: String -> Arg -> String
printArg ind (Arg arg) = printExpression ind arg

printLiteral :: Literal -> String
printLiteral (StringLiteral literal) = show literal
printLiteral (IntegerLiteral literal) = show literal
printLiteral (BooleanLiteral True) = "true"
printLiteral (BooleanLiteral False) = "false"
printLiteral (FloatLiteral literal) = show literal
printLiteral (ListLiteral literal) = "list"
printLiteral (MapLiteral literal) = "map"

printSymdef :: Symdef -> String
printSymdef (Symdef symdef) = symdef

printSymref :: Symref -> String
printSymref (Symref symref) = symref

printNestedBlock :: String -> Block -> String
printNestedBlock ind (Block allparams@(param:params) expr) = "{" ++ (printParams allparams) ++ ": " ++ (printExpression ind expr) ++ "}"
printNestedBlock ind (Block [] expr) = "{" ++ printExpression ind expr ++ "}"

--data LiveBlock = LiveBlock LexicalContext DynamicContext Block deriving (Eq, Show, Typeable, Data)

--data Value =
--      LiteralValue Literal
--    | ComputationValue Computation
--    | MonadValue Monad
--    | ProgramValue LiteralProgram
--    deriving (Eq, Show, Typeable, Data)

--data Computation = Computation LiveBlock
--data Monad =
--      EventMonad [Event]
--    | MutationMonad [Mutation]
--    | ComboMonad EventMonad MutationMonad
--    deriving (Eq, Show, Typeable, Data)

--data Event = Event String deriving (Eq, Show, Typeable, Data)
--data Mutation = Mutation String deriving ()

--load :: LiteralProgram -> LiveBlock

--eval :: LiveBlock -> Value

