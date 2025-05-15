module Generation.EnvTable (buildTraitTable, TraitTable) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Parser.AST (AbsMethodDef, ProgramItem (..), TraitDef (..))

-- | map from trait­name → its list of abstract methods
type TraitTable = Map String [AbsMethodDef]

-- | Build a table of all trait signatures in the program
buildTraitTable :: [ProgramItem] -> TraitTable
buildTraitTable =
  foldr insertTrait Map.empty
 where
  insertTrait (PI_Trait t) tbl =
    Map.insert (traitName t) (traitAbsMethodDef t) tbl
  insertTrait _ tbl = tbl
