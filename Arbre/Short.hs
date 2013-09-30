module Arbre.Short
(
    num
)
where

import Arbre.Program
import Arbre.Box
import Arbre.Native
import Arbre.Objects

num :: Integer -> Expression
num i = do
    let lit = IntegerLit i
    let state = LiteralState lit
    let obj = Object state numdef
    ObjectExp obj

