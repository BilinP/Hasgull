{-|
Module      : Generation.EnvTable
Description : Environment tables for traits and variables in Hasgull.

This module provides functions to build lookup tables:
  
  * 'TraitTable', mapping a trait name to its abstract methods.
  * 'VarTable', mapping a variable name to its corresponding expression as declared by let statements.
-}
module Generation.EnvTable
  ( buildTraitTable
  , TraitTable
  , buildVarTable
  , VarTable
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Parser.AST (AbsMethodDef, ProgramItem (..), TraitDef (..), Expr (..), Stmt (LetStmt), Param (..))

-- | 'TraitTable' is a mapping from a trait name to its list of abstract methods.
type TraitTable = Map String [AbsMethodDef]

-- | 'VarTable' is a mapping from a variable name to the expression it is bound to.
type VarTable = Map String Expr

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

{-|
'buildVarTable' constructs a table of variable declarations from a list of statements.

It traverses the statements and collects all declarations made with 'LetStmt',
binding each variable name to its defining expression.
-}
buildVarTable :: [Stmt] -> VarTable
buildVarTable =
  foldr insertVar Map.empty
 where
    insertVar (LetStmt (Param name _) expr) vbl = 
      Map.insert name expr vbl
    insertVar _ vbl = vbl