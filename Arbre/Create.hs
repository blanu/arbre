import qualified Data.Map as M
import Text.JSON
import Text.JSON.Generic

import Arbre.Program

main = do
    let fname = FunctionName "testLiteral"
    let p1 = Param "x"
    let params = [p1]
    let block = Block params (Simple (BlockExpression $ Block [] (Simple (LiteralExpression $ IntegerLiteral 0) None)) None)
    let def1 = Def fname block
    let defs = [def1]
    let lp = LiteralProgram defs

    let s = encode $ toJSON lp
    putStrLn s
    decodeAndPrint s

    s2 <- readFile "test.json"
    decodeAndPrint s2

decodeAndPrint s = do
    let result = decode s
    case result of
        Ok json -> do
            let lp = (fromJSON json)::(Result LiteralProgram)
            case lp of
                Ok val -> do
                    putStrLn $ "Program: " ++ (show val)
                    printProgDefs val
                Error err2 -> do
                    print err2
        Error err -> do
            print err

printProgDefs (LiteralProgram defs) = printDefs defs

printDefs [] = return ()
printDefs (def:defs) = do
    printDef def
    printDefs defs

printDef (Def (FunctionName s) _) = do
    print s

--    contents <- readFile "test.json"
--    let json = decode contents :: Result (JSObject JSValue)
--    case json of
--        Ok value -> do
--            let arr = fromJSObject value
--            let tree = jsonToProgram value
--            print tree
--        Error error -> do
--            print error

--(!) :: JSON a => JSObject JSValue -> String -> Result a
--(!) = flip valFromObj

--makeStatus tweet = let (!) = flip valFromObj in do
--    userObject <- tweet ! "user"
--    user <- userObject ! "screen_name"
--    text <- tweet ! "text"
--    return Status {user = user, text = text}

--jsonToProgram :: JSObject JSValue -> LiteralProgram
--jsonToProgram obj = LiteralProgram $ jsonToDefs obj

--jsonToDefs :: (JSObject JSValue) -> (M.Map String Def)
--jsonToDefs obj =
--    let arr = fromJSObject obj
--        jsmap = M.fromList arr
--    in M.mapWithKey jsonToDef jsmap

--jsonToDef :: String -> (JSObject JSValue) -> Def
--jsonToDef key value =
--    let Ok jsparams = value ! "params"
--        params = jsonToParams jsparams
--    in Def (FunctionName key) params (Block $ LiteralExpression $ IntegerLiteral 0)

--jsonToDef (YStr key) (YMap value) =
--    let fname = FunctionName name
--        params = jsonToParams ((read paramString)::JSValue)
--        block = jsonToBlock ((read blockString)::JSValue)
--    in Def fname params block

--jsonToParams :: JSValue -> [Param]
--jsonToParams JSArray values = jslistToParams values

--jslistToParams :: [JSValue] -> [Param]
--jslistToParams [] = []
--jslistToParams ((JSString str):values) =
--    Param (fromJSString str) : jslistToParams values

--jsonToBlock :: JSValue -> Block
--jsonToBlock args =
--    let (result,_) = jsonToExpression args
--    in Block result

--jsonToExpression :: JSValue -> (Expression,JSValue)
--jsonToExpression (argTypeString:args) =
--    let argType = (read argTypeString)::Integer
--    in case argType of
--        0 ->
--            let (literal,rest) = jsonToLiteral args
--            in (LiteralExpression literal,rest)
--        1 ->
--            let (literal,rest) = jsonToLiteral args
--            in (Expression literal,rest)
--        --2 ->
--        --    let (literal,rest) = jsonToLiteral args
--        --    in (LiteralExpression literal,rest)

--jsonToLiteral :: JSValue -> (Literal,JSValue)
--jsonToLiteral (argTypeString:argValueString:rest) =
--    let argType = (read argTypeString)::Integer
--    in case argType of
--        0 ->

--        1 ->
--        --2 ->
--        --3 ->
