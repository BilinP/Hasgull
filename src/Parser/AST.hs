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
  , StructActualParam(..)
  ) where


-- Define the expression data type
data Expr
  = Identifier String
  | Int Int
  | NewStruct Type [Param]
  | Negative Expr
  | Add Expr Expr
  | DotExpr Expr Expr
  | Call Expr [Expr]
  | Sub Expr Expr
  | LowerSelf 
  | Multiply Expr Expr
  | Division Expr Expr       
  | Equals Expr Expr         
  | NotEquals Expr Expr      
  | GreaterThan Expr Expr     
  | LessThan Expr Expr                                      
  deriving (Eq, Ord, Show)

data Stmt 
  = LetStmt Param Expr
  | AssgStmt Expr Expr
  | WhileStmt Expr Stmt
  | ForStmt Stmt Expr Stmt Stmt
  | IfStmt Expr Stmt (Maybe Stmt)
  | BreakStmt
  | PrintLnStmt Expr
  | BlockStmt [Stmt]
  | ReturnStmt (Maybe Expr)
  | ExprStmt Expr 
  deriving(Eq, Ord, Show)

data Type 
  = IntType
  | VoidType
  | BooleanType
  | SelfType
  | StructName String
  | HigherOrderType [Type] Type
  deriving(Eq, Ord, Show)
  
data Param 
  = Param String Type
  deriving(Eq, Ord, Show)

data StructActualParam
  = StructActualParam String Expr
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