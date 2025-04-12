module AST where


data Program = Program
  { progItems :: [ProgramItem]
  , progStmts :: [Statement]
  } deriving (Show, Eq)

data ProgramItem
  = PI_Struct StructDef
  | PI_Trait  TraitDef
  | PI_Impl   ImplDef
  | PI_Func   FuncDef
  deriving (Show, Eq)

data TraitDef = TraitDef
  { traitName    :: String
  , traitAbsMethodDef :: [AbsMethodDef]
  } deriving (Show, Eq)

data StructDef = TraitDef
  { structName    :: Type
  , traitAbsMethodDef :: [AbsMethodDef]
  } deriving (Show, Eq)

data AbsMethodDef = AbsMethodDef
  { abMethName :: String
  , abMethParameters :: [CommaParam]
  , abMethReturnType :: Type
  } deriving (Show, Eq)

data StructDef = StructDef
  { strucName   :: String              
  , strucFields :: CommaParam             
  } deriving (Show, Eq)

data ImplDef = ImplDef
  { implTraitName  :: String           -- ^ Which trait we’re implementing
  , iForType    :: TypeDef          -- ^ For what type we implement it
  , iMethods    :: [ConcMethodDef]  -- ^ Concrete method definitions
  } deriving (Show, Eq)

data ConcMethodDef = ConcMethodDef
  { cmName       :: String          -- ^ Method name
  , cmParameters :: CommaParam
  , cmReturnType :: TypeDef
  , cmBody       :: [Statement]     -- ^ A list of statements as the method body
  } deriving (Show, Eq)

  data FuncDef = FuncDef
  { funcName       :: String           -- ^ The function name
  , funcParameters :: CommaParam          -- ^ Parameters
  , fReturnType :: TypeDef
  , fBody       :: [Statement]      -- ^ A list of statements in the function body
  } deriving (Show, Eq)
