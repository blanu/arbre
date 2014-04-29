module Arbre.Load
(
  loadProgram,
  loadViews
)
where

import System.FilePath
import Text.JSON
import Text.JSON.Generic

import Arbre.Expressions hiding (Error)
import Arbre.View

loadProgram :: FilePath -> IO(Maybe ObjectDef)
loadProgram path = do
    s2 <- readFile "test.json"
    return (decodeProgram s2)

decodeProgram :: String -> Maybe ObjectDef
decodeProgram s = do
    let result = decode s
    case result of
        Ok json -> do
            let lp = (fromJSON json)::(Result ObjectDef)
            case lp of
                Ok value -> Just value
                Error err  -> Nothing
        Error err -> Nothing

loadViews :: FilePath -> IO(Maybe Views)
loadViews path = do
    s2 <- readFile "view.json"
    return (decodeViews s2)

decodeViews :: String -> Maybe Views
decodeViews s = do
    let result = decode s
    case result of
        Ok json -> do
            let lp = (fromJSON json)::(Result Views)
            case lp of
                Ok value -> Just value
                Error err  -> Nothing
        Error err -> Nothing
