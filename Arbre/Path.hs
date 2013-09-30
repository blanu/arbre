{-# LANGUAGE DeriveDataTypeable #-}

module Arbre.Path
(
  SourcePath(..),
  Path(..)
)
where

import Data.Typeable
import Data.Data
import Data.Map as M

import Arbre.Program

data SourcePath = SourcePath String Path
data Path = Path [Literal]

-- resolve :: LiteralProgram -> Path ->
