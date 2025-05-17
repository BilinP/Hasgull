{- |
Module      : Parser.AST
Description : The Abstract Syntax Tree (AST) for the Hasgull programming language.

This module defines the core AST data types used in Hasgull.
-}
module Parser.AST (
  Expr (..),
  Type (..),
  Param (..),
  Stmt (..),
  Program (..),
  ProgramItem (..),
  TraitDef (..),
  AbsMethodDef (..),
  StructDef (..),
  ImplDef (..),
  ConcMethodDef (..),
  FuncDef (..),
  StructActualParam (..),
)
where

{- |
The 'Expr' data type represents expressions in the Hasgull language.
-}
data Expr
  = -- | A variable or identifier.
    Identifier String
  | -- | An integer literal.
    Int Int
  | -- | Handles True
    Trueish
  | -- | Handles False
    Falseish
  | -- | Creates a new struct instance.
    NewStruct Type [StructActualParam]
  | -- | Negation of an expression.
    Negative Expr
  | -- | Addition of two expressions.
    Add Expr Expr
  | -- | Field or method access.
    DotExpr Expr Expr
  | -- | Function or method call.
    Call Expr [Expr]
  | -- | Subtraction of two expressions.
    Sub Expr Expr
  | -- | The special 'self' keyword.
    LowerSelf
  | -- | Multiplication of two expressions.
    Multiply Expr Expr
  | -- | Division of two expressions.
    Division Expr Expr
  | -- | Equality comparison.
    Equals Expr Expr
  | -- | Inequality comparison.
    NotEquals Expr Expr
  | -- | Greater-than comparison.
    GreaterThan Expr Expr
  | -- | Less-than comparison.
    LessThan Expr Expr
  deriving (Eq, Ord, Show)

{- |
The 'Stmt' data type represents statements in the Hasgull language.
-}
data Stmt
  = -- | Variable declaration with initialization.
    LetStmt Param Expr
  | -- | Assignment of an expression to a variable.
    AssgStmt Expr Expr
  | -- | While-loop; executes a statement while the condition holds.
    WhileStmt Expr Stmt
  | -- | For-loop with initialization, condition, update, and body.
    ForStmt Stmt Expr Stmt Stmt
  | -- | If-statement with an optional else clause.
    IfStmt Expr Stmt (Maybe Stmt)
  | -- | Break out of a loop.
    BreakStmt
  | -- | Print an expression to the console.
    PrintLnStmt Expr
  | -- | A block of statements.
    BlockStmt [Stmt]
  | -- | Return statement with an optional expression.
    ReturnStmt (Maybe Expr)
  | -- | An expression used as a statement.
    ExprStmt Expr
  deriving (Eq, Ord, Show)

{- |
The 'Type' data type represents types in the Hasgull language.
-}
data Type
  = -- | The integer type.
    IntType
  | -- | The void type.
    VoidType
  | -- | The boolean type.
    BooleanType
  | -- | The special 'Self' type.
    SelfType
  | -- | A named struct type.
    StructName String
  | -- | A higher-order function type with parameter types and a return type.
    HigherOrderType [Type] Type
  deriving (Eq, Ord, Show)

{- |
The 'Param' data type represents parameters (for functions or declarations) with a name and type.
-}
data Param
  = -- | A parameter with a name and an associated type.
    Param String Type
  deriving (Eq, Ord, Show)

{- |
The 'StructActualParam' data type represents an actual parameter used when creating a struct instance.
-}
data StructActualParam
  = -- | A field initialization with a field name and an expression.
    StructActualParam String Expr
  deriving (Eq, Ord, Show)

{- |
The 'Program' data type represents an entire Hasgull program.
-}
data Program = Program
  { progItems :: [ProgramItem]
  -- ^ Top-level program items such as struct, trait, or function definitions.
  , progStmts :: [Stmt]
  -- ^ Top-level statements.
  }
  deriving (Show, Eq)

{- |
The 'ProgramItem' data type represents items that can appear at the top level in a program.
-}
data ProgramItem
  = -- | A struct definition.
    PI_Struct StructDef
  | -- | A trait (typeclass) definition.
    PI_Trait TraitDef
  | -- | An implementation of a trait.
    PI_Impl ImplDef
  | -- | A function definition.
    PI_Func FuncDef
  deriving (Show, Eq)

{- |
The 'TraitDef' data type represents a trait (or typeclass) definition.
-}
data TraitDef = TraitDef
  { traitName :: String
  -- ^ The name of the trait.
  , traitAbsMethodDef :: [AbsMethodDef]
  -- ^ The abstract methods declared by the trait.
  }
  deriving (Show, Eq)

{- |
The 'AbsMethodDef' data type represents an abstract method definition within a trait.
-}
data AbsMethodDef = AbsMethodDef
  { abMethName :: String
  -- ^ The name of the abstract method.
  , abMethParameters :: [Param]
  -- ^ The parameters of the method.
  , abMethReturnType :: Type
  -- ^ The return type of the method.
  }
  deriving (Show, Eq)

{- |
The 'StructDef' data type represents a struct definition.
-}
data StructDef = StructDef
  { strucName :: String
  -- ^ The name of the struct.
  , strucFields :: [Param]
  -- ^ The fields of the struct.
  }
  deriving (Show, Eq)

{- |
The 'ImplDef' data type represents an implementation of a trait for a specific type.
-}
data ImplDef = ImplDef
  { implTraitName :: String
  -- ^ The name of the trait being implemented.
  , iForType :: Type
  -- ^ The type for which the trait is implemented.
  , iMethods :: [ConcMethodDef]
  -- ^ The concrete method definitions that implement the trait.
  }
  deriving (Show, Eq)

{- |
The 'ConcMethodDef' data type represents a concrete method definition.
-}
data ConcMethodDef = ConcMethodDef
  { cmName :: String
  -- ^ The name of the concrete method.
  , cmParameters :: [Param]
  -- ^ The parameters for the method.
  , cmReturnType :: Type
  -- ^ The return type of the method.
  , cmBody :: [Stmt]
  -- ^ The body of the method, expressed as a list of statements.
  }
  deriving (Show, Eq)

{- |
The 'FuncDef' data type represents a top-level function definition.
-}
data FuncDef = FuncDef
  { funcName :: String
  -- ^ The function name.
  , funcParameters :: [Param]
  -- ^ The parameters of the function.
  , funcReturnType :: Type
  -- ^ The return type of the function.
  , funcBody :: [Stmt]
  -- ^ The function body as a list of statements.
  }
  deriving (Show, Eq)