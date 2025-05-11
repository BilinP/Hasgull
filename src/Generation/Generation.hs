module Generation.Generation
    ( 
        translateStmt, translateType, translateParam, translateExpr
    ) where

import Parser.AST
import Data.List(intercalate)



-- Code Generation stuff
-- Since our "target" langauge is javascript, I'm honestly just testing first if we can take an AST and 
-- write to a string, then idk put into a javascript file   .



translateType :: Type -> String
translateType t = case t of 
    IntType -> "int"
    VoidType -> "void"
    BooleanType -> "boolean"
    SelfType -> "Self"
    StructName name -> name
    HigherOrderType types returnType -> 
        let typesStr = intercalate "," (map translateType types)
            returnTypeStr = translateType returnType
        in typesStr ++ " => " ++ returnTypeStr



translateExpr :: Expr -> String
translateExpr expr = case expr of
    Identifier name -> name
    Int n -> show n
    Negative e -> "-" ++ translateExpr e
    Add e1 e2 -> translateExpr e1 ++ " + " ++ translateExpr e2
    DotExpr e1 e2 -> translateExpr e1 ++ "." ++ translateExpr e2
    Call e args -> translateExpr e ++ "(" ++  intercalate "," (map translateExpr args) ++ ")"
    Sub e1 e2 -> translateExpr e1 ++ " - " ++ translateExpr e2
    LowerSelf -> "self"
    Multiply e1 e2 -> translateExpr e1 ++ " * " ++ translateExpr e2
    Division e1 e2 -> translateExpr e1 ++ " / " ++ translateExpr e2
    Equals e1 e2 -> translateExpr e1 ++ " === " ++ translateExpr e2
    NotEquals e1 e2 -> translateExpr e1 ++ " !== " ++ translateExpr e2
    GreaterThan e1 e2 -> translateExpr e1 ++ " > " ++ translateExpr e2
    LessThan e1 e2 -> translateExpr e1 ++ " < " ++ translateExpr e2


translateParam :: Param -> String
translateParam (Param name t) = --Since we are converting to javascript, then why would we include the type? The paraser should already have done the check on typing
    name

translateStmt :: Stmt -> String
translateStmt stmt = case stmt of
    BreakStmt -> "break;"
    LetStmt param e -> "let" ++ " " ++ translateParam param ++ " = " ++ translateExpr e
    AssgStmt e1 e2 -> translateExpr e1 ++ " = " ++ translateExpr e2
    BlockStmt stmts -> "{ " ++ intercalate " \n " (map translateStmt stmts) ++ " }" --default show in test means that it just prints the newline character
    WhileStmt e stmts -> "while(" ++ translateExpr e ++ ") " ++ translateStmt stmts  
    IfStmt e st maybeElse -> "if(" ++ translateExpr e ++ ") " ++ translateStmt st ++ case maybeElse of
        Just elseStmt -> "  else " ++ translateStmt elseStmt
        Nothing -> ""
    ForStmt stmt1 e1 stmt2 stmt3 -> "for( " ++ translateStmt stmt1 ++ " ; " ++ translateExpr e1 ++ " ; " ++ translateStmt stmt2 ++ " ) " ++ translateStmt stmt3
    ReturnStmt maybeExpr -> "return " ++ case maybeExpr of
        Just reExpr -> translateExpr reExpr ++ ";"
        Nothing -> ";"
    PrintLnStmt e -> "console.log( " ++ translateExpr e ++ ")"
    _ -> "How did you get here?"


