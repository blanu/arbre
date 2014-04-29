module Arbre.Mutation
(
  applyMutation
)
where

import Arbre.Context
import Arbre.Expressions

applyMutation :: Expression -> Context -> Context
applyMutation (Mutation Define (Symdef sym) value) context =
  bindPair Dyn context (sym, value)
applyMutation (Mutation Set (Symdef sym) value) context =
  bindPair Dyn context (sym, value)
