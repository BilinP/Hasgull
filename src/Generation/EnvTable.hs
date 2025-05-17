
module Generation.EnvTable (buildTraitTable, TraitTable) where

{-|
Module      : Generation.EnvTable
Description : Environment tables for traits and variables in Hasgull.

This module provides functions to build lookup tables:
  
  * 'TraitTable', mapping a trait name to its abstract methods.
  * 'VarTable', mapping a variable name to its corresponding expression as declared by let statements.
-}

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Parser.AST (AbsMethodDef, Expr (..), Param (..), ProgramItem (..), Stmt (LetStmt), TraitDef (..))

-- | 'TraitTable' is a mapping from a trait name to its list of abstract methods.
type TraitTable = Map String [AbsMethodDef]


{-|
'buildTraitTable' constructs a table of trait signatures from a list of program items.

It traverses the program items and, for every trait definition (wrapped in 'PI_Trait'),
inserts an entry mapping the trait's name to its list of abstract methods.
-}
buildTraitTable :: [ProgramItem] -> TraitTable
buildTraitTable =
  foldr insertTrait Map.empty
 where
  insertTrait (PI_Trait t) tbl =
    Map.insert (traitName t) (traitAbsMethodDef t) tbl
  insertTrait _ tbl = tbl
