module Parser.AST
  ( Expr(..)
  , Type(..)
  , Param(..)
  , Stmt(..)
  , Program(..)
  , ProgramItem(..)
  , TraitDef(..)
  , AbsMethodDef(..)
  , StructDef(..)
  , ImplDef(..)
  , ConcMethodDef(..)
  , FuncDef(..)
  ) where


-- Define the expression data type
data Expr
  = Identifier String
  | Int Int
  | Negative Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Multiply Expr Expr
  | Division Expr Expr
  | If Expr Expr (Maybe Expr) 
  | While Expr Expr          
  | Equals Expr Expr         
  | NotEquals Expr Expr      
  | GreaterThan Expr Expr     
  | LessThan Expr Expr                 
  | Return Expr               
  | PrintLn Expr              
  deriving (Eq, Ord, Show)

data Type 
  = IntType
  | VoidType
  | BooleanType
  | SelfType
  | StructName String
  | CommaType Type [Type] -- ?
  | HigherOrderType Type Type
  deriving(Eq, Ord, Show)

data Param 
  = Param String Type
  | CommaParam Param [Param]
  deriving(Eq, Ord, Show)

data Stmt 
  = LetStmt Param Expr
  | AssgStmt Expr Expr
  | BreakStmt
  | BlockStmt [Stmt]
  | ExprStmt Expr 
  deriving(Eq, Ord, Show)

data Program = Program
  { progItems :: [ProgramItem]
  , progStmts :: [Stmt]
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

data AbsMethodDef = AbsMethodDef
  { abMethName :: String
  , abMethParameters :: Param
  , abMethReturnType :: Type
  } deriving (Show, Eq)

--Param
data StructDef = StructDef
  { strucName   :: String              
  , strucFields :: Param             
  } deriving (Show, Eq)

data ImplDef = ImplDef
  { implTraitName  :: String           
  , iForType    :: Type          
  , iMethods    :: [ConcMethodDef]  
  } deriving (Show, Eq)

data ConcMethodDef = ConcMethodDef
  { cmName       :: String          
  , cmParameters :: Param
  , cmReturnType :: Type
  , cmBody       :: [Stmt]     
  } deriving (Show, Eq)

data FuncDef = FuncDef
    { funcName       :: String
    , funcParameters :: Param
    , funcReturnType    :: Type
    , funcBody          :: [Stmt]
    } deriving (Show, Eq)