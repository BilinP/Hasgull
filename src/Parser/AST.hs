module AST where

data TraitDef = TraitDef
  { tName    :: String
  , tMethods :: [AbsMethodDef]
  } deriving (Show, Eq)

data AbsMethodDef = AbsMethodDef
  { mName :: String
  } deriving (Show, Eq)
