module Generation.EnvTable (buildTraitTable, TraitTable,buildVarTable,VarTable) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Parser.AST (AbsMethodDef, ProgramItem (..), TraitDef (..), Expr (..), Stmt (LetStmt), Param (..))

-- | map from trait­name → its list of abstract methods
type TraitTable = Map String [AbsMethodDef]
type VarTable = Map String Expr

-- | Build a table of all trait signatures in the program
buildTraitTable :: [ProgramItem] -> TraitTable
buildTraitTable =
  foldr insertTrait Map.empty
 where
  insertTrait (PI_Trait t) tbl =
    Map.insert (traitName t) (traitAbsMethodDef t) tbl
  insertTrait _ tbl = tbl

--Hopefully build a table of all created variables by let statements, so we can have some form of checker.
buildVarTable :: [Stmt] -> VarTable
buildVarTable =
  foldr insertVar Map.empty
 where
    insertVar (LetStmt (Param name _) expr) vbl = 
      Map.insert name expr vbl
    insertVar _ vbl = vbl