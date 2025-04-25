module Parser.Parser (
  parseExpression,
  parseType,
  parseParam,
  parseStmt,
  pTraitDef,
  pAbsMethodDef,
  pStructDef,
  pImplDef,
  pConcMethodDef,
  pFuncDef,
  pProgramItem,
  pProgram
) where

import Tokenizer.Token (Token(..))
import Control.Monad.Combinators.Expr
import Control.Applicative
import Text.Megaparsec hiding (Token, many) -- Hide Token to avoid ambiguity
import Text.Megaparsec.Char()
import Data.Void
import Parser.AST



-- Define the parser type
type Parser = Parsec Void [Token]
 

pCommaExp :: Parser [Expr]
pCommaExp = option [] $ do
    first <- pExpr
    rest  <- many (symbol CommaToken *> pExpr)
    return (first : rest)

--Call expression parser
pCallExp :: Parser Expr
pCallExp = do
  func <- pSingleTerm
  args <- many (between (symbol LParenToken) (symbol RParenToken) pCommaExp)
  pure (foldl Call func args) --Pure is a wrapper that succeeds without consuming input, at least that's what the documentation says





-- Parse a variable
pVariable :: Parser Expr
pVariable = Identifier <$> (satisfy isIdentifierToken >>= \(IdentifierToken name) -> pure name)

isIdentifierToken :: Token -> Bool
isIdentifierToken (IdentifierToken _) = True
isIdentifierToken _ = False


-- Parse an integer
pInteger :: Parser Expr
pInteger = Int <$> (satisfy isIntegerToken >>= \(IntegerToken value) -> pure value)

isIntegerToken :: Token -> Bool
isIntegerToken (IntegerToken _) = True
isIntegerToken _ = False

-- checkMatching Token
checkMatchingToken :: Token -> Parser Token
checkMatchingToken t = label (show t) $ satisfy (== t)
---------------------------------------------------------------------------
pAtom :: Parser Expr
pAtom = choice [ pParensAtom, pVariable, pInteger, pBoolean ]

pParensAtom :: Parser Expr
pParensAtom = between (symbol LParenToken) (symbol RParenToken) pExpr

pReturnStmt :: Parser Stmt
pReturnStmt = ReturnStmt <$> (symbol ReturnToken *> (optional pAtom) <* symbol SemiColonToken)

pPrintLnStmt :: Parser Stmt
pPrintLnStmt = PrintLnStmt <$> (symbol PrintLnToken *> (between (symbol LParenToken) (symbol RParenToken) pAtom) ) <*  symbol SemiColonToken
---------------------------------------------------------------------------


--- Parse type literals

pType :: Parser Type
pType = try pFunctionType <|> pAtomType -- Try if it's a higher order type, backtrack and try the other parse if not

pAtomType :: Parser Type
pAtomType = choice 
   [ pIntType
   , pVoidType
   , pBooleanType
   , pSelfType
   , pStructname
   , pParenType
   ]

pFunctionType :: Parser Type
pFunctionType = do
  arg <- between (symbol LParenToken) (symbol RParenToken) pArgType --Argument of the higher order function
  symbol ArrowToken 
  result <- pType
  return $ HigherOrderType arg result -- Bind the HigherOrder Type with the argument and the resulting type

pArgType :: Parser [Type]
pArgType = option [] $ do
  firstArg <- pAtomType --Get the single,guranteed type
  restArgs <- many (symbol CommaToken *> pAtomType)
  return ( firstArg : restArgs)

pIntType :: Parser Type
pIntType = IntType <$ symbol IntToken 

pVoidType :: Parser Type
pVoidType = VoidType <$ symbol VoidToken

pBooleanType :: Parser Type
pBooleanType = BooleanType <$ symbol BooleanToken

pSelfType :: Parser Type
pSelfType = SelfType <$ symbol SelfToken

pStructname :: Parser Type
pStructname = StructName <$> (satisfy isIdentifierToken >>= \(IdentifierToken name) -> pure name)

pParenType :: Parser Type
pParenType = between (symbol LParenToken) (symbol RParenToken) pType

parseParam :: [Token] -> Either (ParseErrorBundle [Token] Void ) Param
parseParam = runParser pParam ""

parseStmt :: [Token] -> Either (ParseErrorBundle [Token] Void) Stmt
parseStmt = runParser pStmt ""

------------------------------------------------------

pParam :: Parser Param
pParam = try pAtomParam -- Parse through and see if there's any comma token in the stream, backtrack if failed and do atomParam

pCommaParam :: Parser [Param]
pCommaParam = option [] $ do
  firstParam <- pAtomParam -- Get the first param, guranteeded
  restParams <- many (symbol CommaToken *> pAtomParam) -- Return a list of Params, we don't care about the Comma token so its a list
  return (firstParam : restParams)

pAtomParam :: Parser Param
pAtomParam = Param
        <$> (satisfy isIdentifierToken >>= \(IdentifierToken name) -> pure name)
        <* symbol ColonToken
        <*> pType
       


--- Parse Statements
pLetStmt :: Parser Stmt
pLetStmt = LetStmt <$> (symbol LetToken *> pParam)
                   <*> (symbol EqualToken *> pExpr <* symbol SemiColonToken)

pAssgStmt :: Parser Stmt
pAssgStmt = AssgStmt <$> (pExpr <* symbol EqualToken)
                     <*> (pExpr <* symbol SemiColonToken)

pAssgStmtSemiLess :: Parser Stmt
pAssgStmtSemiLess = AssgStmt <$> (pExpr <* symbol EqualToken)
                     <*> pExpr 
                     

pExprStmt :: Parser Stmt
pExprStmt = ExprStmt <$> (pExpr <* symbol SemiColonToken)     



pBreakStmt :: Parser Stmt
pBreakStmt = BreakStmt <$ (symbol BreakToken <* symbol SemiColonToken)

pBlockStmt :: Parser Stmt
pBlockStmt = BlockStmt <$> (symbol LBraceToken *> (many pStmt) <* symbol RBraceToken)

pWhileStmt :: Parser Stmt
pWhileStmt = WhileStmt <$> (symbol WhileToken *> pCondition)
               <*> pBlockStmt



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
  


pStmt :: Parser Stmt
pStmt = choice 
         [
           pLetStmt,
           pAssgStmt,
           pIfStmt,
           pWhileStmt,
           pForStmt,
           pExprStmt,
           pBreakStmt,
           pPrintLnStmt,
           pBlockStmt,
           pReturnStmt
         ]
        


-- Parse a boolean literal
pBoolean :: Parser Expr
pBoolean = (Identifier "true" <$ symbol TrueToken) <|> (Identifier "false" <$ symbol FalseToken)

-- Parse a self expression.
pSelf :: Parser Expr
pSelf = LowerSelf <$ symbol LowerCaseSelfToken



-- Parse parentheses
pParens :: Parser Expr
pParens = between (symbol LParenToken) (symbol RParenToken) pExpr

-- Parse an if expression
pIfStmt :: Parser Stmt
pIfStmt = IfStmt <$> (symbol IfToken *> pCondition)
         <*> pStmt 
         <*> optional pElseOrElseIf

-- Parse an else or else if block #quirk 
pElseOrElseIf :: Parser Stmt
pElseOrElseIf = do
  _ <- symbol ElseToken
  choice
    [ 
      IfStmt <$> (symbol IfToken *> pCondition)
         <*> (symbol LBraceToken *> pStmt <* symbol RBraceToken)
         <*> optional pElseOrElseIf
    , 
       pStmt 
    ]


-- Parse a condition (boolean or comparison expression)
pCondition :: Parser Expr
pCondition = makeExprParser condTerm condOperatorTable

-- Parse conditions enclosed in parentheses
pParensCondition :: Parser Expr
pParensCondition = between (symbol LParenToken) (symbol RParenToken) pCondition

condTerm :: Parser Expr
condTerm = choice
  [ pBoolean
  , pVariable
  , pInteger
  , pParensCondition
  , pSelf
  
  ]

condOperatorTable :: [[Operator Parser Expr]]
condOperatorTable =
  [ [ binary EqualsToken Equals
    , binary NotEqualToken NotEquals
    , binary GreaterThanToken GreaterThan
    , binary LessThanToken LessThan
    ]
  ]

pTerm :: Parser Expr
pTerm = pCallExp <|> pSingleTerm

pSingleTerm :: Parser Expr
pSingleTerm = choice
  [ pParens 
  , pInteger
  , pSelf
  , pVariable
  ]

-- Parse an expression
pExpr :: Parser Expr
pExpr =  makeExprParser pTerm operatorTable

-- TABLE
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix SubtractToken Negative ,
      binary DotToken DotExpr
    ]
  , [ binary MultiplyToken Multiply
    , binary DivideToken Division
    ]
  , [ binary AddToken Add
    , binary SubtractToken Sub
    ]
  , [ binary EqualsToken Equals
    , binary NotEqualToken NotEquals
    , binary GreaterThanToken GreaterThan
    , binary LessThanToken LessThan
    ]
  ]

-- Helper for binary operators
binary :: Token -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary tok f = InfixL (f <$ symbol tok)


-- Helper for prefix operators
prefix :: Token -> (Expr -> Expr) -> Operator Parser Expr
prefix tok f = Prefix (f <$ symbol tok)

-- Parse a specific token
symbol :: Token -> Parser Token
symbol t = satisfy (== t)

-- Entry point for parsing expressions
parseExpression :: [Token] -> Either (ParseErrorBundle [Token] Void) Expr
parseExpression = runParser pExpr ""

parseType :: [Token] -> Either (ParseErrorBundle [Token] Void) Type
parseType = runParser pType ""

---------------------------------------------------------------------------------
--NOTES I did the opposite in terms of naming scheme from you guys
-- this was by accident

-- | Parse a TraitDef from a list of tokens
pTraitDef :: [Token] -> Either (ParseErrorBundle [Token] Void) TraitDef
pTraitDef = runParser parseTraitDef ""

-- | Parse an AbsMethodDef from a list of tokens
pAbsMethodDef :: [Token] -> Either (ParseErrorBundle [Token] Void) AbsMethodDef
pAbsMethodDef = runParser parseAbsMethodDef ""

-- | Parse a StructDef from a list of tokens
pStructDef :: [Token] -> Either (ParseErrorBundle [Token] Void) StructDef
pStructDef = runParser parseStructDef ""

-- | Parse an ImplDef from a list of tokens
pImplDef :: [Token] -> Either (ParseErrorBundle [Token] Void) ImplDef
pImplDef = runParser parseImplDef ""

-- | Parse a ConcMethodDef from a list of tokens
pConcMethodDef :: [Token] -> Either (ParseErrorBundle [Token] Void) ConcMethodDef
pConcMethodDef = runParser parseConcMethodDef ""

-- | Parse a FuncDef from a list of tokens
pFuncDef :: [Token] -> Either (ParseErrorBundle [Token] Void) FuncDef
pFuncDef = runParser parseFuncDef ""

-- | Parse a ProgramItem from a list of tokens
pProgramItem :: [Token] -> Either (ParseErrorBundle [Token] Void) ProgramItem
pProgramItem = runParser parseProgramItem ""

-- | Parse a Program from a list of tokens
pProgram :: [Token] -> Either (ParseErrorBundle [Token] Void) Program
pProgram = runParser parseProgram ""
-----------------------------------------------------------------------------------------


pIdentifier :: Parser String
pIdentifier = do
  tok <- satisfy isIdentifierToken
  case tok of
    IdentifierToken name -> pure name
    _ -> fail "Expected identifier"

pAtomStructActualParam :: Parser StructActualParam
pAtomStructActualParam = StructActualParam
        <$> (satisfy isIdentifierToken >>= \(IdentifierToken name) -> pure name)
        <* symbol ColonToken
        <*> pExpr

pStructActualParam :: Parser StructActualParam
pStructActualParam = try pAtomStructActualParam

pStructActualParams :: Parser [StructActualParam]
pStructActualParams = option [] $ do
  firstParam <- pAtomStructActualParam
  restParams <- many (symbol CommaToken *> pAtomStructActualParam)
  return (firstParam : restParams)

-- Trait Parser
parseTraitDef :: Parser TraitDef
parseTraitDef =
  TraitDef
    <$ checkMatchingToken TraitToken
    <*> pIdentifier
    <* checkMatchingToken LBraceToken
    <*> many parseAbsMethodDef
    <* checkMatchingToken RBraceToken

-- Abstract method Definition Parser
parseAbsMethodDef :: Parser AbsMethodDef
parseAbsMethodDef =
  AbsMethodDef
    <$ checkMatchingToken MethodToken
    <*> pIdentifier
    <* checkMatchingToken LParenToken
    <*> pParam
    <* checkMatchingToken RParenToken
    <* checkMatchingToken ColonToken
    <*> pType
    <* checkMatchingToken SemiColonToken


-- Structdef parser
-- structdef ::= `struct` structname `{` comma_param `}`
parseStructDef :: Parser StructDef
parseStructDef =
  StructDef
    <$ checkMatchingToken StructToken
    <*> pIdentifier
    <* checkMatchingToken LBraceToken
    <*> pParam
    <* checkMatchingToken RBraceToken

-- ImplDef Parser
-- impldef ::= `impl` traitname `for` type `{` conc_methoddef* `}`
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

-- ImplDef Parser
-- conc_methoddef ::= `method` var `(` comma_param `)` `:` type `{` stmt* `}`
parseConcMethodDef :: Parser ConcMethodDef
parseConcMethodDef =
  ConcMethodDef
    <$ checkMatchingToken MethodToken
    <*> pIdentifier
    <* checkMatchingToken LParenToken
    <*> pParam
    <* checkMatchingToken RParenToken
    <* checkMatchingToken ColonToken
    <*> pType
    <* checkMatchingToken LBraceToken
    <*> many pStmt
    <* checkMatchingToken RBraceToken

-- FuncDef Parser
-- funcdef ::= `func` var `(` comma_param `)` `:` type `{` stmt* `}`
parseFuncDef :: Parser FuncDef
parseFuncDef =
  FuncDef
    <$ checkMatchingToken FuncToken
    <*> pIdentifier
    <* checkMatchingToken LParenToken
    <*> pParam
    <* checkMatchingToken RParenToken
    <* checkMatchingToken ColonToken
    <*> pType
    <* checkMatchingToken LBraceToken
    <*> many pStmt
    <* checkMatchingToken RBraceToken

-- parse Program Items
-- program_item ::= structdef | traitdef | impldef | funcdef
parseProgramItem :: Parser ProgramItem
parseProgramItem =
  choice
    [ PI_Struct <$> parseStructDef
    , PI_Trait  <$> parseTraitDef
    , PI_Impl   <$> parseImplDef
    , PI_Func   <$> parseFuncDef
    ]

-- parse Program
-- program ::= program_item* stmt* 
parseProgram :: Parser Program
parseProgram =
  Program
    <$> many parseProgramItem
    <*> many pStmt

 