module Arbre.Short
(
    num,
    float,
    bool,
    string,
    block,
    modul,
    self,
    local,
    env,
    dyn,
    true,
    false,
    define,
    set,
    combine,
    superdefine,
    superset,
    prnt,
    stdin
)
where

import Arbre.Expressions
import Arbre.Box
import Arbre.Native
import Arbre.Objects

num :: Integer -> Expression
num i = do
    let lit = IntegerLit i
    let state = LiteralState lit
    let obj = Object state numdef
    ObjectExp obj

float :: Float -> Expression
float i = do
    let lit = FloatLit i
    let state = LiteralState lit
    let obj = Object state numdef
    ObjectExp obj

bool :: Bool -> Expression
bool i = do
    let lit = BooleanLit i
    let state = LiteralState lit
    let obj = Object state numdef
    ObjectExp obj

string :: String -> Expression
string i = do
    let lit = StringLit i
    let state = LiteralState lit
    let obj = Object state stringdef
    ObjectExp obj

true :: Expression
true = bool True

false :: Expression
false = bool False

block :: [String] -> Expression -> Expression
block name call = BlockExp $ Block name call

modul :: [Def] -> Expression
modul defs = ObjectExp $ Module $ ObjectDef defs

self :: String -> Expression
self name = Symref Self name

local :: String -> Expression
local name = Symref Local name

env :: String -> Expression
env name = Symref Lex name

dyn :: String -> Expression
dyn name = Symref Dyn name

define :: String -> Expression -> Expression
define sym value = Mutation Define (Symdef sym) value

set :: String -> Expression -> Expression
set sym value = Mutation Set (Symdef sym) value

superdefine :: [(String, Integer)] -> Expression
superdefine defs = combine (map definePair defs)

superset :: [(String, Expression)] -> Expression
superset defs = combine (map setPair defs)

definePair :: (String, Integer) -> Expression
definePair (k,v) = define k (num v)

setPair :: (String, Expression) -> Expression
setPair (k,v) = set k v

combine :: [Expression] -> Expression
combine (d:d2:[]) = Combine d d2
combine (d:defs) = Combine d $ combine defs

prnt :: Expression -> Expression
prnt exp = Event Print exp

stdin :: Expression -> Expression
stdin exp = Receiver Stdin $ block ["input"] exp
