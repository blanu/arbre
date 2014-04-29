{-# LANGUAGE DeriveDataTypeable #-}

module Arbre.Event
(
  applyEvent
)
where

import Arbre.Expressions
import Arbre.Print
import Arbre.View
import Arbre.Native

applyEvent :: EventType -> Expression -> IO()
applyEvent Print value = do
  let maybeString = unboxString value
  case maybeString of
    Just s  -> putStrLn s
    Nothing -> putStrLn $ printExpression (Views []) "" value
