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

-- Code Generation stuff
-- Since our "target" langauge is javascript, I'm honestly just testing first if we can take an AST and
-- write to a string, then idk put into a javascript file

-- Thoughts
-- Have lists constructed at block stmts or equivalents that get passed into translate calls?
-- If it's a letstmt or some form of initilizer, we push into list
-- Any call or AST part that isn't an immediate, we check that list, throw exception if not.

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
translateExpr :: Expr -> String
translateExpr expr = case expr of
  Identifier name -> name
  Int n -> show n
  Negative e -> "-" ++ translateExpr e
  Add e1 e2 -> translateExpr e1 ++ "+" ++ translateExpr e2
  DotExpr e1 e2 -> translateExpr e1 ++ "." ++ translateExpr e2
  Call e args -> translateExpr e ++ "(" ++ intercalate "," (map translateExpr args) ++ ")"
  Sub e1 e2 -> translateExpr e1 ++ "-" ++ translateExpr e2
  LowerSelf -> "self"
  Multiply e1 e2 -> translateExpr e1 ++ "*" ++ translateExpr e2
  Division e1 e2 -> translateExpr e1 ++ "/" ++ translateExpr e2
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

-- Translate a Stmt AST node into a string of an equivalent javascript expression
translateStmt :: Stmt -> String
translateStmt stmt = case stmt of
  BreakStmt -> "break;"
  LetStmt param e -> "let" ++ " " ++ translateParam param ++ " = " ++ translateExpr e ++ ";"
  AssgStmt e1 e2 -> translateExpr e1 ++ "=" ++ translateExpr e2 ++ "; "
  BlockStmt stmts -> "{ " ++ intercalate " \n " (map translateStmt stmts) ++ "} " -- default show in test means that it just prints the newline character
  WhileStmt e stmts -> "while(" ++ translateExpr e ++ ") " ++ translateStmt stmts
  IfStmt e st maybeElse ->
    "if( " ++ translateExpr e ++ ") " ++ translateStmt st ++ case maybeElse of
      Just elseStmt -> " else " ++ translateStmt elseStmt
      Nothing -> ""
  ForStmt stmt1 e1 stmt2 stmt3 -> "for(" ++ translateStmt stmt1 ++ ";" ++ translateExpr e1 ++ " ; " ++ translateStmt stmt2 ++ ")" ++ translateStmt stmt3
  ReturnStmt maybeExpr ->
    "return " ++ case maybeExpr of
      Just reExpr -> translateExpr reExpr ++ ";" ++ " "
      Nothing -> ";"
  PrintLnStmt e -> "console.log(" ++ translateExpr e ++ ")" ++ ";"
  ExprStmt e1 -> translateExpr e1 ++ ";"
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
    -- pull names out of each Param
    paramList = intercalate ", " [pname | Param pname _ <- params]

    -- open the function
    header = "function " ++ name ++ "(" ++ paramList ++ ") {\n"

    -- emit the body exactly as statements
    stmts = concatMap translateStmt body

    -- close it
    footer = "}\n\n"
   in
    header ++ stmts ++ footer

translateImpl :: TraitTable -> ImplDef -> String
translateImpl tbl (ImplDef traitName forType methods) =
  case Map.lookup traitName tbl of
    Nothing ->
      error $ "Unknown trait: " ++ traitName
    Just absMeths ->
      let
        -- method names required by the trait
        required :: [String]
        required = [name | AbsMethodDef name _params _retTy <- absMeths]

        -- method names you actually implemented
        provided :: [String]
        provided = [name | ConcMethodDef name _params _retTy _body <- methods]

        -- any that are still missing
        missing :: [String]
        missing = required \\ provided
       in
        if not (null missing)
          then
            error $
              "Impl for trait “"
                ++ traitName
                ++ "” on "
                ++ show forType
                ++ " is missing methods: "
                ++ show missing
          else concatMap (emitMethod forType) methods
 where
  emitMethod (StructName t) (ConcMethodDef mName params _ body) =
    let args = intercalate ", " [n | Param n _ <- params]
        header = t ++ ".prototype." ++ mName ++ " = function(" ++ args ++ ") {\n"
        stmts = concatMap translateStmt body
        footer = "};\n\n"
     in header ++ stmts ++ footer
  emitMethod _ _ = ""

translateItem :: TraitTable -> ProgramItem -> String
translateItem traitTbl = \case
  PI_Struct s -> translateStruct s
  PI_Trait _ -> "" -- will fill in later
  PI_Impl impl -> translateImpl traitTbl impl
  PI_Func func -> translateFunc func
