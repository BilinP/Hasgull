module Generation.EnvTable (

) where

import Data.Map (Map)
import qualified Data.Map as Map
import Parser.AST

import qualified Data.Map.Strict as Map -- from the containers package

-- | We key each definition by its name (or, for impls, by (traitName, targetType))
data Env = Env
  { structs :: Map.Map String StructDef
  , traits :: Map.Map String TraitDef
  , impls :: Map.Map (String, Type) ImplDef
  , funcs :: Map.Map String FuncDef
  }

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty Map.empty Map.empty

-- | Fold each top-level AST item into the environment
addItem :: Env -> ProgramItem -> Env
addItem env = \case
  PI_Struct s ->
    env{structs = Map.insert (strucName s) s (structs env)}
  PI_Trait t ->
    env{traits = Map.insert (traitName t) t (traits env)}
  PI_Impl i ->
    let key = (implTraitName i, iForType i)
     in env{impls = Map.insert key i (impls env)}
  PI_Func f ->
    env{funcs = Map.insert (funcName f) f (funcs env)}

-- | Build the full environment from your parsed program
buildEnv :: Program -> Env
buildEnv (Program items _) = foldl addItem emptyEnv items
