-- |
-- Module      : Parser.Parser
-- Description : Parsers for the Hasgull programming language.
--
-- This module provides parsers for expressions, types, parameters, statements, and
-- program structures used in the Hasgull language. It also includes parsers for trait,
-- struct, implementation, and function definitions.
module Parser.Parser where

import Control.Applicative
import Control.Monad.Combinators.Expr
-- Hide Token to avoid ambiguity
import Data.Void
import Parser.AST
import Text.Megaparsec hiding (Token, many)
import Text.Megaparsec.Char ()
import Tokenizer.Token (Token (..))

-- | Parser type alias for a token stream using Megaparsec.
type Parser = Parsec Void [Token]

-- | Parse a comma-separated list of expressions.
pCommaExp :: Parser [Expr]
pCommaExp = option [] $ do
  first <- pExpr
  rest <- many (symbol CommaToken *> pExpr)
  return (first : rest)

-- | Parse a function or method call expression.
pCallExp :: Parser Expr
pCallExp = do
  func <- pSingleTerm
  lookAhead (symbol LParenToken) -- Only succeed if next token is LParenToken
  args <- between (symbol LParenToken) (symbol RParenToken) pCommaExp
  pure (Call func args)

-- | Parse a variable reference (identifier) as an expression.
pVariable :: Parser Expr
pVariable = Identifier <$> (satisfy isIdentifierToken >>= \(IdentifierToken name) -> pure name)

-- | Predicate to determine if a token is an identifier.
isIdentifierToken :: Token -> Bool
isIdentifierToken (IdentifierToken _) = True
isIdentifierToken _ = False

-- | Parse an integer literal.
pInteger :: Parser Expr
pInteger = Int <$> (satisfy isIntegerToken >>= \(IntegerToken value) -> pure value)

-- | Predicate to determine if a token is an integer.
isIntegerToken :: Token -> Bool
isIntegerToken (IntegerToken _) = True
isIntegerToken _ = False

-- | Parse a struct instantiation expression.
pNewStruct :: Parser Expr
pNewStruct = do
  _ <- symbol NewToken
  structer <- pStructname
  _ <- symbol LBraceToken
  structParams <- pStructActualParams
  _ <- symbol RBraceToken
  return $ NewStruct structer structParams

-- | Check that a token matches the expected token.
checkMatchingToken :: Token -> Parser Token
checkMatchingToken t = label (show t) $ satisfy (== t)

---------------------------------------------------------------------------
-- Expression Parsers

-- | Parse an atomic expression including parentheses, identifiers, integers,
-- booleans, or struct instantiation.
pAtom :: Parser Expr
pAtom = choice [pParensAtom, pVariable, pInteger, pBoolean, pNewStruct]

-- | Parse an expression enclosed in parentheses.
pParensAtom :: Parser Expr
pParensAtom = between (symbol LParenToken) (symbol RParenToken) pExpr

-- | Parse a return statement.
pReturnStmt :: Parser Stmt
pReturnStmt = ReturnStmt <$> (symbol ReturnToken *> (optional pExpr) <* symbol SemiColonToken)

-- | Parse a print statement.
pPrintLnStmt :: Parser Stmt
pPrintLnStmt = PrintLnStmt <$> (symbol PrintLnToken *> pParensAtom) <* symbol SemiColonToken

---------------------------------------------------------------------------

--- Type Parsers

-- | Parse a type literal. Tries the function type parser first; on failure,
-- falls back to parsing an atomic type.
pType :: Parser Type
pType = try pFunctionType <|> pAtomType

-- | Parse an atomic type.
pAtomType :: Parser Type
pAtomType =
  choice
    [ pIntType,
      pVoidType,
      pBooleanType,
      pSelfType,
      pStructname,
      pParenType
    ]

-- | Parse a higher-order (function) type.
pFunctionType :: Parser Type
pFunctionType = do
  arg <- between (symbol LParenToken) (symbol RParenToken) pArgType -- Argument types
  symbol ArrowToken
  result <- pType
  return $ HigherOrderType arg result

-- | Parse a comma-separated list of argument types.
pArgType :: Parser [Type]
pArgType = option [] $ do
  firstArg <- pAtomType
  restArgs <- many (symbol CommaToken *> pAtomType)
  return (firstArg : restArgs)

-- | Parse the integer type.
pIntType :: Parser Type
pIntType = IntType <$ symbol IntToken

-- | Parse the void type.
pVoidType :: Parser Type
pVoidType = VoidType <$ symbol VoidToken

-- | Parse the boolean type.
pBooleanType :: Parser Type
pBooleanType = BooleanType <$ symbol BooleanToken

-- | Parse the 'Self' type.
pSelfType :: Parser Type
pSelfType = SelfType <$ symbol SelfToken

pStructname :: Parser Type
pStructname = StructName <$> (satisfy isIdentifierToken >>= \(IdentifierToken name) -> pure name)

-- | Parse a parenthesized type.
pParenType :: Parser Type
pParenType = between (symbol LParenToken) (symbol RParenToken) pType

---------------------------------------------------------------------------
-- Parameter Parsers

-- | Parse a parameter.
pParam :: Parser Param
pParam = try pAtomParam

-- | Parse a comma-separated list of parameters.
pCommaParam :: Parser [Param]
pCommaParam = option [] $ do
  firstParam <- pAtomParam
  restParams <- many (symbol CommaToken *> pAtomParam)
  return (firstParam : restParams)

-- | Parse a single parameter.
pAtomParam :: Parser Param
pAtomParam =
  Param
    <$> (satisfy isIdentifierToken >>= \(IdentifierToken name) -> pure name)
    <* symbol ColonToken
    <*> pType

--- Statement Parsers

-- | Parse a let statement.
pLetStmt :: Parser Stmt
pLetStmt =
  LetStmt
    <$> (symbol LetToken *> pParam)
    <*> (symbol EqualToken *> pExpr <* symbol SemiColonToken)

-- | Parse an assignment statement.
pAssgStmt :: Parser Stmt
pAssgStmt =
  AssgStmt
    <$> (pExpr <* symbol EqualToken)
    <*> (pExpr <* symbol SemiColonToken)

-- | Parse an assignment statement without a trailing semicolon.
pAssgStmtSemiLess :: Parser Stmt
pAssgStmtSemiLess = AssgStmt <$> (pExpr <* symbol EqualToken) <*> pExpr

-- | Parse an expression used as a statement.
pExprStmt :: Parser Stmt
pExprStmt = ExprStmt <$> pExpr <* symbol SemiColonToken

-- | Parse a break statement.
pBreakStmt :: Parser Stmt
pBreakStmt = BreakStmt <$ (symbol BreakToken <* symbol SemiColonToken)

-- | Parse a block of statements.
pBlockStmt :: Parser Stmt
pBlockStmt = BlockStmt <$> (symbol LBraceToken *> many pStmt <* symbol RBraceToken)

-- | Parse a while statement.
pWhileStmt :: Parser Stmt
pWhileStmt =
  WhileStmt
    <$> (symbol WhileToken *> pCondition)
    <*> pBlockStmt

-- | Parse a for loop statement.
pForStmt :: Parser Stmt
pForStmt = do
  _ <- symbol ForToken
  _ <- symbol LParenToken
  initStmt <- pLetStmt <|> pAssgStmt
  condExpr <- pExpr <* symbol SemiColonToken
  postStmt <- pAssgStmtSemiLess
  _ <- symbol RParenToken
  bodyStmt <- pStmt
  return $ ForStmt initStmt condExpr postStmt bodyStmt

-- | Parse any statement.
pStmt :: Parser Stmt
pStmt =
  choice
    [ pLetStmt,
      try pAssgStmt,
      pIfStmt,
      pWhileStmt,
      pForStmt,
      pBreakStmt,
      pPrintLnStmt,
      pBlockStmt,
      pReturnStmt,
      pExprStmt
    ]

-- | Parse a boolean literal.
pBoolean :: Parser Expr
pBoolean = (Identifier "true" <$ symbol TrueToken) <|> (Identifier "false" <$ symbol FalseToken)

-- | Parse the 'self' expression.
pSelf :: Parser Expr
pSelf = LowerSelf <$ symbol LowerCaseSelfToken

pTrue :: Parser Expr
pTrue = Trueish <$ symbol TrueToken

pFalse :: Parser Expr
pFalse = Falseish <$ symbol FalseToken

-- Parse parentheses
pParens :: Parser Expr
pParens = between (symbol LParenToken) (symbol RParenToken) pExpr

-- | Parse an if statement.
pIfStmt :: Parser Stmt
pIfStmt =
  IfStmt
    <$> (symbol IfToken *> pCondition)
    <*> pStmt
    <*> optional pElseOrElseIf

-- | Parse an else or else-if block.
pElseOrElseIf :: Parser Stmt
pElseOrElseIf = do
  _ <- symbol ElseToken
  choice
    [ IfStmt
        <$> (symbol IfToken *> pCondition)
        <*> (symbol LBraceToken *> pStmt <* symbol RBraceToken)
        <*> optional pElseOrElseIf,
      pStmt
    ]

-- | Parse a condition expression.
pCondition :: Parser Expr
pCondition = makeExprParser condTerm condOperatorTable

-- | Parse an expression followed by a semicolon (used in conditions).
pPainandMisery :: Parser Expr
pPainandMisery = pExpr <* symbol SemiColonToken

-- | Parse a condition enclosed in parentheses.
pParensCondition :: Parser Expr
pParensCondition = between (symbol LParenToken) (symbol RParenToken) pCondition

-- | Parses a conditional term expression in the following order from top to bottom:
condTerm :: Parser Expr
condTerm =
  choice
    [ pBoolean,
      pVariable,
      pInteger,
      pParensCondition,
      pSelf
    ]

-- | Operator table for condition expressions.
condOperatorTable :: [[Operator Parser Expr]]
condOperatorTable =
  [ [ binary EqualsToken Equals,
      binary NotEqualToken NotEquals,
      binary GreaterThanToken GreaterThan,
      binary LessThanToken LessThan
    ]
  ]

-- | Parse a term expression (either a call expression or a single term).
pTerm :: Parser Expr
pTerm = try pCallExp <|> pSingleTerm

-- | Parse a single term expression.
pSingleTerm :: Parser Expr
pSingleTerm =
  choice

    [ pParens
    , pInteger
    , pSelf
    , pVariable
    , pTrue
    , pFalse
    , pNewStruct
    ]

-- | Parse an expression.
pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix SubtractToken Negative,
      binary DotToken DotExpr
    ],
    [ binary MultiplyToken Multiply,
      binary DivideToken Division
    ],
    [ binary AddToken Add,
      binary SubtractToken Sub
    ],
    [ binary EqualsToken Equals,
      binary NotEqualToken NotEquals,
      binary GreaterThanToken GreaterThan,
      binary LessThanToken LessThan
    ]
  ]

-- | Helper for binary operators.
binary :: Token -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary tok f = InfixL (f <$ symbol tok)

-- | Helper for prefix operators.
prefix :: Token -> (Expr -> Expr) -> Operator Parser Expr
prefix tok f = Prefix (f <$ symbol tok)

-- | Parse a specific token.
symbol :: Token -> Parser Token
symbol t = satisfy (== t)

---------------------------------------------------------------------------
-- Exported Parsing Functions

-- | Parse an expression from a list of tokens.
parseExpression :: [Token] -> Either (ParseErrorBundle [Token] Void) Expr
parseExpression = runParser pExpr ""

-- | Parse a type from a list of tokens.
parseType :: [Token] -> Either (ParseErrorBundle [Token] Void) Type
parseType = runParser pType ""

-- | Parse a parameter from a list of tokens.
parseParam :: [Token] -> Either (ParseErrorBundle [Token] Void) Param
parseParam = runParser pParam ""

-- | Parse a statement from a list of tokens.
parseStmt :: [Token] -> Either (ParseErrorBundle [Token] Void) Stmt
parseStmt = runParser pStmt ""

-- | Parse a TraitDef from a list of tokens.
pTraitDef :: [Token] -> Either (ParseErrorBundle [Token] Void) TraitDef
pTraitDef = runParser parseTraitDef ""

-- | Parse an AbsMethodDef from a list of tokens.
pAbsMethodDef :: [Token] -> Either (ParseErrorBundle [Token] Void) AbsMethodDef
pAbsMethodDef = runParser parseAbsMethodDef ""

-- | Parse a StructDef from a list of tokens.
pStructDef :: [Token] -> Either (ParseErrorBundle [Token] Void) StructDef
pStructDef = runParser parseStructDef ""

-- | Parse an ImplDef from a list of tokens.
pImplDef :: [Token] -> Either (ParseErrorBundle [Token] Void) ImplDef
pImplDef = runParser parseImplDef ""

-- | Parse a ConcMethodDef from a list of tokens.
pConcMethodDef :: [Token] -> Either (ParseErrorBundle [Token] Void) ConcMethodDef
pConcMethodDef = runParser parseConcMethodDef ""

-- | Parse a top-level function definition from a list of tokens.
pFuncDef :: [Token] -> Either (ParseErrorBundle [Token] Void) FuncDef
pFuncDef = runParser parseFuncDef ""

-- | Parse a ProgramItem from a list of tokens.
pProgramItem :: [Token] -> Either (ParseErrorBundle [Token] Void) ProgramItem
pProgramItem = runParser parseProgramItem ""

-- | Parse an entire Program from a list of tokens.
pProgram :: [Token] -> Either (ParseErrorBundle [Token] Void) Program
pProgram = runParser parseProgram ""

---------------------------------------------------------------------------
-- Trait / Method / Struct / Implementation Parsers

-- | check if Identifier 'token' for  struct name.
pIdentifier :: Parser String
pIdentifier = do
  tok <- satisfy isIdentifierToken
  case tok of
    IdentifierToken name -> pure name
    _ -> fail "Expected identifier"

-- | Parse a struct name.
pAtomStructActualParam :: Parser StructActualParam
pAtomStructActualParam =
  StructActualParam
    <$> (satisfy isIdentifierToken >>= \(IdentifierToken name) -> pure name)
    <* symbol ColonToken
    <*> pExpr

-- | Parse a single struct actual parameter.
pStructActualParam :: Parser StructActualParam
pStructActualParam = try pAtomStructActualParam

-- | Parse a comma-separated list of struct actual parameters.
pStructActualParams :: Parser [StructActualParam]
pStructActualParams = option [] $ do
  firstParam <- pAtomStructActualParam
  restParams <- many (symbol CommaToken *> pAtomStructActualParam)
  return (firstParam : restParams)

-- | Parse a trait definition.
parseTraitDef :: Parser TraitDef
parseTraitDef =
  TraitDef
    <$ checkMatchingToken TraitToken
    <*> pIdentifier
    <* checkMatchingToken LBraceToken
    <*> many parseAbsMethodDef
    <* checkMatchingToken RBraceToken

-- | Parse an abstract method definition.
parseAbsMethodDef :: Parser AbsMethodDef
parseAbsMethodDef =
  AbsMethodDef
    <$ checkMatchingToken MethodToken
    <*> pIdentifier
    <* checkMatchingToken LParenToken
    <*> pCommaParam
    <* checkMatchingToken RParenToken
    <* checkMatchingToken ColonToken
    <*> pType
    <* checkMatchingToken SemiColonToken

-- | Parse a struct definition.
parseStructDef :: Parser StructDef
parseStructDef =
  StructDef
    <$ checkMatchingToken StructToken
    <*> pIdentifier
    <* checkMatchingToken LBraceToken
    <*> pCommaParam
    <* checkMatchingToken RBraceToken

-- | Parse an implementation definition.
parseImplDef :: Parser ImplDef
parseImplDef =
  ImplDef
    <$ checkMatchingToken ImplToken
    <*> pIdentifier
    <* checkMatchingToken ForToken
    <*> pType
    <* checkMatchingToken LBraceToken
    <*> many parseConcMethodDef
    <* checkMatchingToken RBraceToken

-- | Parse a concrete method definition.
parseConcMethodDef :: Parser ConcMethodDef
parseConcMethodDef =
  ConcMethodDef
    <$ checkMatchingToken MethodToken
    <*> pIdentifier
    <* checkMatchingToken LParenToken
    <*> pCommaParam
    <* checkMatchingToken RParenToken
    <* checkMatchingToken ColonToken
    <*> pType
    <* checkMatchingToken LBraceToken
    <*> many pStmt
    <* checkMatchingToken RBraceToken

-- | Parse a function definition.
parseFuncDef :: Parser FuncDef
parseFuncDef =
  FuncDef
    <$ checkMatchingToken FuncToken
    <*> pIdentifier
    <* checkMatchingToken LParenToken
    <*> pCommaParam
    <* checkMatchingToken RParenToken
    <* checkMatchingToken ColonToken
    <*> pType
    <* checkMatchingToken LBraceToken
    <*> many pStmt
    <* checkMatchingToken RBraceToken

-- | Parse a top-level program item.
parseProgramItem :: Parser ProgramItem
parseProgramItem =
  choice
    [ PI_Struct <$> parseStructDef,
      PI_Trait <$> parseTraitDef,
      PI_Impl <$> parseImplDef,
      PI_Func <$> parseFuncDef
    ]

-- | Parse an entire program consisting of program items and statements.
parseProgram :: Parser Program
parseProgram =
  Program
    <$> many parseProgramItem
    <*> many pStmt
