{-# LANGUAGE DeriveDataTypeable #-}

module Arbre.NewPrint
(
    printProg,
    printExpression,
    printObjectFull,
    printArgs,
    printComputation,
    printComputationContext,
    printContext,
    printMapping,
    printClosure,
    printClosureContext
)
where

import qualified Data.Map as M
import qualified Data.List as L

import Arbre.Program
import Arbre.Box
import Arbre.View
import Arbre.Context
import Arbre.Native

data Chapter = Chapter [Paragraph]
data Paragraph = Paragraph [Word] [Paragraph]
data Word =
    Whitespace
  | Subject String
  | Arg String
  | Clause [Word]
  | Sugar String deriving (Eq, Show)

printObjectFull :: Object -> [Chapter]
printObjectFull (Module defMap) = printObjectDef defMap
--printObjectFull views ind (Object state defMap) = show state ++ ": " ++ printObjectDef views defMap

printObjectDef :: ObjectDef -> [Chapter]
printObjectDef (ObjectDef defs) = map printDef defs

printDef :: Def -> Chapter
printDef (Def name defExpr) =
  Chapter $ Paragraph [Subject name, Sugar ":"] [printExpression defExpr]

printExpression :: Expression -> Paragraph
printExpression (ObjectExp obj) = Paragraph (printObject obj) []
printExpression (Symdef symdef) = Paragraph (printSymdef symdef) []
printExpression (Symref env sym) = Paragraph (printSymref env sym) []
printExpression (Mutation Define (Symdef sym) value) =
  Paragraph (printSymdef sym ++ Subject ":=") [printExpression value]
printExpression (Mutation Set (Symdef sym) value) =
  Paragraph (printSymdef sym ++ Subject "=") [printExpression value]
printExpression (Error message) =
  Paragraph [Sugar "<error: ", Subject message, Sugar ">"] [] False False
printExpression (BlockExp block) = printNestedBlock block
printExpression call@(Call _ _) = printCall call
printExpression (Apply clos@(Closure _ _ _ _ _) params) =
  printClosure clos ++ " . (" ++ printArgs params ++ ")"
printExpression clos@(Closure _ _ _ _ _) = printClosure clos
printExpression call@(NativeCall _ _) = printNativeCall call  
printExpression (Event Print value) = "io <- " ++ printExpression value
printExpression (Receiver Stdin block@(BlockExp (Block args exp))) = "io -> " ++ printParams args ++ " do " ++ printExpression block
printExpression (Combine a b) = (printExpression a) ++ "; " ++ printExpression b
printExpression exp = "<print not implemented>" ++ show exp

printObject :: Object -> [Word]
printObject (Module mod) = [Subject "<module>"]
printObject (Object state def) = [Sugar "@", Subject "object"]

printSymdef :: String -> [Word]
printSymdef symdef = [printEnvironment Dyn, Subject symdef]

printSymref :: Environment -> String -> [Word]
printSymref env sym = [printEnvironment env, Subject sym]

printEnvironment :: Environment -> Word
printEnvironment Lex = Sugar "^"
printEnvironment Dyn = Sugar "$"
printEnvironment Self = Sugar "~"
printEnvironment Value = Sugar "&"
printEnvironment Local = Sugar ":"

printParams :: [String] -> String
printParams params = L.intercalate ", " params

printBlock :: Views -> String -> Expression -> String
printBlock expr = printExpression expr

printClosure :: Views -> String -> Expression -> String
printClosure (Closure lex dyn self value block) =
  "{...|" ++ printNestedBlock block ++ "}"

printClosureContext :: Views -> String -> Expression -> String
printClosureContext (Closure lex dyn self value block) =
  "{" ++ printMapping Lex lex ++ ", " ++ printMapping Dyn dyn ++ ", " ++ printMapping Self self ++ "," ++ printMapping Value value ++ "|" ++ printNestedBlock block ++ "}"

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

printCall :: Expression -> String
printCall call@(Call (Symref env name) args) =
--    case (getView views name) of
--        Just view -> printCallWithView view call
--        Nothing -> name ++ "(" ++ (printArgs args) ++ ")"
  name ++ "(" ++ (printArgs args) ++ ")"
printCall exp = "<unimplemented call> " ++ show exp

--printCallWithView :: Views -> String -> View -> Expression -> String
--printCallWithView (View name []) call@(Call _ args) = ""
--printCallWithView (View name (part:parts)) call@(Call _ args) =
--    printViewPartWithCall part args ++ " " ++ printCallWithView (View name parts) call

printNativeCall :: Views -> String -> Expression -> String
printNativeCall call@(NativeCall native args) = do
    let name = nativeName native
--    case (getView views name) of
--        Just view -> printCallWithView view call
--        Nothing -> "#" ++ name ++ "(" ++ (printArgs args) ++ ")"
  "#" ++ name ++ "(" ++ (printArgs args) ++ ")"
printNativeCall exp = "<unimplemented native call> " ++ show exp

printViewPartWithCall :: Views -> String -> ViewPart -> [Expression] -> String
printViewPartWithCall (SymrefView index) args =
    let arg = args !! index
    in printArg arg
printViewPartWithCall (SymdefView index) args =
    let arg = args !! index
    in printArg arg
--printViewPartWithCall (BlockView index) args =
--    let arg = args !! index
--    in printIndentedBlockArg views ("  ") arg
printViewPartWithCall (FuncView str) args = str
printViewPartWithCall (SugarView str) args = str

--printIndentedBlockArg :: Views -> String -> Expression -> String
--printIndentedBlockArg arg =
--    case arg of
--        BlockExp block ->
--            printIndentedBlock block
--        otherwise ->
--            "Type Error: Block required for view"

printIndentedBlock :: Views -> String -> Block -> String
printIndentedBlock (Block _ block) =
    "\n" ++ printBlock block ++ "\n"

printArgs :: Views -> String -> [Expression] -> String
printArgs args = L.intercalate ", " (map (printArg views ind) args)

printArg :: Views -> String -> Expression -> String
printArg arg = printExpression arg

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
printNestedBlock (Block allparams@(param:params) expr) = "{with " ++ (printParams allparams) ++ ": " ++ (printExpression expr) ++ "}"
printNestedBlock (Block [] expr) = "{" ++ printExpression expr ++ "}"
