{-# LANGUAGE DeriveDataTypeable #-}

module Arbre.View
(
    Views(..),
    View(..),
    ViewPart(..),

    getView
)
where

import Data.Typeable
import Data.Data

data Views = Views [View] deriving (Eq, Show, Typeable, Data)
data View = View ViewName [ViewPart] deriving (Eq, Show, Typeable, Data)
data ViewName = ViewName String deriving (Eq, Show, Typeable, Data)
data ViewPart =
      SymrefView Int
    | SymdefView Int
    | BlockView Int
    | FuncView String
    | SugarView String
    deriving (Eq, Show, Typeable, Data)

getView :: Views -> String -> Maybe View
getView (Views []) name = Nothing
getView (Views (view@(View (ViewName viewName) _):views)) name =
    if viewName == name
        then Just view
        else getView (Views views) name

