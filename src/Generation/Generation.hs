{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Generation.Generation (
  generateJS,
  translateStmt,
  translateType,
  translateParam,
  translateExpr,
  createOutputFile,
) where

import Prelude

import Data.List (intercalate, (\\))
import qualified Data.Map as Map
import Generation.EnvTable (TraitTable, buildTraitTable)
import Parser.AST
import System.IO (readFile, writeFile)


-- | generateJS: Converts a Program AST to JavaScript.
generateJS :: Program -> String
generateJS (Program items stmts) =
  let
    traitTbl = buildTraitTable items
    itemJS = concatMap (translateItem traitTbl) items
    stmtJS = concatMap translateStmt stmts
   in
    itemJS ++ stmtJS

-- generateJS program =
--  let progJS = concatMap translateProgramItem (progItems program)
--     stmtsJS = concatMap translateStmt (progStmts program)
-- in progJS ++ stmtsJS

-- translateProgramItem :: ProgramItem -> String
-- translateProgramItem item = case item of
--  PI_Func funcdef -> translateFuncDef funcdef
--  _ -> ""

translateBlock :: [Stmt] -> String
translateBlock stmts = "{" ++ intercalate " " (map translateStmt stmts) ++ "}"

-- Code Generation stuff


-- Current set Generator. Due to time limitations, we can't exactly handle higher order type for function declarations
-- Or have some of variable checker, it still compiles but it's undefined behavior and in javascript's hands by then.AddToken

-- Test function to see if we can write the output of a generateJS to a test javascript file

createOutputFile :: Program -> String -> IO String
createOutputFile output outputFilename = do
  let theOutputFile = outputFilename ++ ".js"
  writeFile theOutputFile (generateJS output)
  return "Successfully Compilied!"

-- Translate Type
-- translate an Type AST node into a string of an equivalent javascript expression
translateType :: Type -> String
translateType t = case t of
  IntType -> "number"
  VoidType -> "void"
  BooleanType -> "boolean"
  SelfType -> "this"
  StructName name -> name
  HigherOrderType types returnType ->
    let typesStr = intercalate ", " (map translateType types) -- Intercalate is used just so we can do type, type, type
        returnTypeStr = translateType returnType
     in typesStr ++ " => " ++ returnTypeStr

-- Translate an expression AST node into a string of an equivalent javascript expression
-- idea, do the look up table stuff for the first expression and second in add, sub, divide, multiply 
-- Because 

translateExpr :: Expr -> String
translateExpr expr = case expr of
  Identifier name -> name
  Int n -> show n
  Negative e -> "-" ++ translateExpr e
  Add e1 e2 -> translateExpr e1 ++ "+" ++ translateExpr e2
  DotExpr e1 e2 -> translateExpr e1 ++ "." ++ translateExpr e2
  Call e args -> translateExpr e ++ "(" ++ intercalate "," (map translateExpr args) ++ ")"
  Sub e1 e2 -> translateExpr e1 ++ "-" ++ translateExpr e2
  LowerSelf -> "this"
  Trueish -> "true"
  Falseish -> "false"
  Multiply e1 e2 ->
    let wrap e = case e of
          Add _ _ -> "(" ++ translateExpr e ++ ")"
          Sub _ _ -> "(" ++ translateExpr e ++ ")"
          _ -> translateExpr e
     in wrap e1 ++ "*" ++ wrap e2
  Division e1 e2 ->
    let wrap e = case e of
          Add _ _ -> "(" ++ translateExpr e ++ ")"
          Sub _ _ -> "(" ++ translateExpr e ++ ")"
          _ -> translateExpr e
     in wrap e1 ++ "/" ++ wrap e2
  Equals e1 e2 -> translateExpr e1 ++ "===" ++ translateExpr e2
  NotEquals e1 e2 -> translateExpr e1 ++ "!==" ++ translateExpr e2
  GreaterThan e1 e2 -> translateExpr e1 ++ ">" ++ translateExpr e2
  LessThan e1 e2 -> translateExpr e1 ++ "<" ++ translateExpr e2
  NewStruct t1 ps -> "new " ++ translateType t1 ++ "(" ++ intercalate "," (map translateStructParam ps) ++ ")"

translateStructParam :: StructActualParam -> String
translateStructParam sparam = case sparam of
  StructActualParam str e -> translateExpr e

-- Translate a Param AST node into a string of an equivalent javascript expression
translateParam :: Param -> String
translateParam (Param name t) =
  -- Since we are converting to javascript, then why would we include the type? The paraser should already have done the check on typing
  name

wrapBlock :: Stmt -> String
wrapBlock stmt = case stmt of
  BlockStmt stmts -> translateBlock stmts
  _ -> translateStmt stmt

-- Helper function for the assgStmt in the For Loop
translateIterForLoop :: Stmt -> String
translateIterForLoop val = case val of
  AssgStmt e1 e2 -> translateExpr e1 ++ "=" ++ translateExpr e2
  _ -> "err"

-- Translate a Stmt AST node into a string of an equivalent javascript expression
translateStmt :: Stmt -> String
translateStmt stmt = case stmt of
  BreakStmt -> "break;"
  LetStmt param e -> "let" ++ " " ++ translateParam param ++ " = " ++ translateExpr e ++ ";"
  AssgStmt e1 e2 -> translateExpr e1 ++ "=" ++ translateExpr e2 ++ "; "
  BlockStmt stmts -> translateBlock stmts
  WhileStmt e stmts -> "while(" ++ translateExpr e ++ ") " ++ wrapBlock stmts
  IfStmt e st maybeElse ->
    "if( "
      ++ translateExpr e
      ++ ") "
      ++ wrapBlock st
      ++ case maybeElse of
        Just elseStmt -> " else " ++ wrapBlock elseStmt
        Nothing -> ""
  ForStmt initS evalute update body ->
    "for(" ++ translateStmt initS ++ translateExpr evalute ++ " ; " ++ translateIterForLoop update ++ ")" ++ wrapBlock body
  ReturnStmt maybeExpr ->
    "return " ++ case maybeExpr of
      Just reExpr -> translateExpr reExpr ++ ";" ++ " "
      Nothing -> ";"
  PrintLnStmt e -> "console.log(" ++ translateExpr e ++ ")" ++ ";"
  ExprStmt e -> translateExpr e ++ ";"
  _ -> "How did you get here?"

-- Indent Helper function
indent :: Int -> String -> String
indent n s = replicate (n * 2) ' ' ++ s

-- Translates a StructDef AST node into a string of an equivalent javascript
translateStruct :: StructDef -> String
translateStruct (StructDef name fields) =
  let
    -- extract field names
    fieldNames :: [String]
    fieldNames = [fname | Param fname _ <- fields]

    -- header and constructor
    header = "class " ++ name ++ " {\n"
    consArgs = intercalate ", " fieldNames
    consStart = indent 1 ("constructor(" ++ consArgs ++ ") {") ++ "\n"

    -- one assignment per field
    assigns =
      concatMap
        (\fn -> indent 2 ("this." ++ fn ++ " = " ++ fn ++ ";") ++ "\n")
        fieldNames

    consEnd = indent 1 "}\n"
    footer = "}\n\n"
   in
    header
      ++ consStart
      ++ assigns
      ++ consEnd
      ++ footer

-- Translates a StructDef AST node into a string of an equivalent javascript
translateFunc :: FuncDef -> String
translateFunc (FuncDef name params _ body) =
  let
    paramList = intercalate ", " [pname | Param pname _ <- params]
    header = "function " ++ name ++ "(" ++ paramList ++ ") {"
    stmts = concatMap translateStmt body
    footer = "}"
   in
    header ++ stmts ++ footer

-- Maps a Hasgull Type to its JavaScript constructor/prototype base
jsConstructorName :: Type -> Maybe String
jsConstructorName = \case
  StructName name -> Just name
  IntType -> Just "Number"
  BooleanType -> Just "Boolean"
  _ -> Nothing

-- Emit a prototype method assignment for a type
emitMethod :: Type -> ConcMethodDef -> String
emitMethod typ (ConcMethodDef mName params _ body) =
  case jsConstructorName typ of
    Just target ->
      let
        args = intercalate ", " [n | Param n _ <- params]
        header = target ++ ".prototype." ++ mName ++ " = function(" ++ args ++ ") {"
        stmts = concatMap translateStmt body
        footer = "};"
       in
        header ++ stmts ++ footer
    Nothing -> ""

-- | Translate an implementation of a trait to JavaScript
translateImpl :: TraitTable -> ImplDef -> String
translateImpl tbl (ImplDef traitName forType methods) =
  case Map.lookup traitName tbl of
    Nothing ->
      error $ "Unknown trait: " ++ traitName
    Just absMeths ->
      let
        required = [name | AbsMethodDef name _ _ <- absMeths]
        provided = [name | ConcMethodDef name _ _ _ <- methods]
        missing = required \\ provided
       in
        if not (null missing)
          then
            error $
              "Impl for trait \""
                ++ traitName
                ++ "\" on "
                ++ show forType
                ++ " is missing methods: "
                ++ show missing
          else concatMap (emitMethod forType) methods

translateItem :: TraitTable -> ProgramItem -> String
translateItem traitTbl = \case
  PI_Struct s -> translateStruct s
  PI_Trait _ -> "" -- will fill in later
  PI_Impl impl -> translateImpl traitTbl impl
  PI_Func func -> translateFunc func
