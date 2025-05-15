module Generation.Generation (
  generateJS,
  translateStmt,
  translateType,
  translateParam,
  translateExpr,
  createOutputFile,
) where

import Prelude

import Data.List (intercalate)
import Parser.AST
import System.IO (readFile, writeFile)

-- | generateJS: Converts a Program AST to JavaScript.
generateJS :: Program -> String
generateJS program =
  concatMap translateStmt (progStmts program)

-- Code Generation stuff
-- Since our "target" langauge is javascript, I'm honestly just testing first if we can take an AST and
-- write to a string, then idk put into a javascript file

-- Thoughts
-- Have lists constructed at block stmts or equivalents that get passed into translate calls?
-- If it's a letstmt or some form of initilizer, we push into list
-- Any call or AST part that isn't an immediate, we check that list, throw exception if not.

-- Test function to see if we can write the output of a generateJS to a test javascript file

createOutputFile :: Program -> String -> IO String
createOutputFile output outputFilename= do
    let theOutputFile = outputFilename ++ ".js"
    writeFile theOutputFile  (generateJS output)
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
    Call e args -> translateExpr e ++ "(" ++  intercalate "," (map translateExpr args) ++ ")"
    Sub e1 e2 -> translateExpr e1 ++ "-" ++ translateExpr e2
    LowerSelf -> "self"
    Multiply e1 e2 -> translateExpr e1 ++ "*" ++ translateExpr e2
    Division e1 e2 -> translateExpr e1 ++ "/" ++ translateExpr e2
    Equals e1 e2 -> translateExpr e1 ++ "===" ++ translateExpr e2
    NotEquals e1 e2 -> translateExpr e1 ++ "!==" ++ translateExpr e2
    GreaterThan e1 e2 -> translateExpr e1 ++ ">" ++ translateExpr e2
    LessThan e1 e2 -> translateExpr e1 ++ "<" ++ translateExpr e2
    NewStruct t1 ps -> translateType t1 ++ "(" ++ intercalate "," (map translateStructParam ps) ++ ")"


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
  _ -> "How did you get here?"

-- translateStruct :: StructDef -> String
-- translateStruct (StructDef name fields) =
--   let
--     -- note to self fix Param in StructDef
--     Param fName _fTy = fields

--     header = "class " ++ name ++ " {" ++ "\n"
--     constHeader = indent 1 ("constructor(" ++ fName ++ ") {") ++ "\n"
--     assign = indent 2 ("this." ++ fName ++ " = " ++ fName ++ ";") ++ "\n"
--     ctorEnd = indent 1 "}" ++ "\n"
--     footer = "}" ++ "\n\n"
--    in
--     header
--       ++ constHeader
--       ++ assign
--       ++ ctorEnd
--       ++ footer

-- translateItem :: ProgramItem -> String
-- translateItem = \case
--   PI_Struct sdef -> genStruct sdef
--   PI_Trait _ -> "" -- will fill in later
--   PI_Impl _ -> "" -- will fill in later
--   PI_Func _ -> "" -- will fill in later
