{-# LANGUAGE DeriveDataTypeable #-}

module Arbre.Receiver
(
  applyReceiver
)
where

import Arbre.Expressions
import Arbre.Print
import Arbre.View
import Arbre.Native
import Arbre.Context as C
import Arbre.Box

applyReceiver :: ReceiverType -> Expression -> Context -> IO(Computation)
applyReceiver Stdin closure@(Closure _ _ _ _ _) context  = do
  c <- getChar
  let s = [c] :: String
  let s' = boxString s
  return $ Computation context (Apply closure [s'])
applyReceiver receiver exp context = return $ Computation context (Error $ "Unsupported event type: " ++ show receiver ++ " " ++ show exp)
