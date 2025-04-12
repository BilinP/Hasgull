module Parser.Parser (
  Expr(..),
  Type(..),
  parseExpression,
  parseType
) where

import Tokenizer.Token (Token(..))
import Control.Monad.Combinators.Expr
import Control.Applicative
import Text.Megaparsec hiding (Token) -- Hide Token to avoid ambiguity
import Text.Megaparsec.Char()
import Data.Void

-- Define the parser type
type Parser = Parsec Void [Token]

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
  | CommaType Type Type -- ?
  | HigherType Type Type
  deriving(Eq, Ord, Show)
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



---------------------------------------------------------------------------
pAtom :: Parser Expr
pAtom = choice [ pParensAtom, pVariable, pInteger, pBoolean ]

pParensAtom :: Parser Expr
pParensAtom = between (symbol LParenToken) (symbol RParenToken) pAtom

pReturn :: Parser Expr
pReturn = Return <$> (symbol ReturnToken *> pAtom)

pPrintLn :: Parser Expr
pPrintLn = PrintLn <$> (symbol PrintLnToken *> pAtom)
---------------------------------------------------------------------------


--- Parse type literals

pType :: Parser Type
pType = makeExprParser pTypeTerm operatingTableType

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

pTypeTerm :: Parser Type
pTypeTerm = choice 
   [ pIntType
   , pVoidType
   , pBooleanType
   , pSelfType
   , pStructname
   , pParenType
   ]

operatingTableType :: [[Operator Parser Type]]
operatingTableType =
  [
     [commaType CommaToken CommaType
     ,commaType ArrowToken HigherType
     ]
  ]

-- Parse a boolean literal
pBoolean :: Parser Expr
pBoolean = (Identifier "true" <$ symbol TrueToken) <|> (Identifier "false" <$ symbol FalseToken)


-- Parse parentheses
pParens :: Parser Expr
pParens = between (symbol LParenToken) (symbol RParenToken) pExpr

-- Parse an if expression
pIf :: Parser Expr
pIf = If <$> (symbol IfToken *> pCondition)
         <*> (symbol LBraceToken *> pExpr <* symbol RBraceToken)
         <*> optional pElseOrElseIf

-- Parse an else or else if block #quirk 
pElseOrElseIf :: Parser Expr
pElseOrElseIf = do
  _ <- symbol ElseToken
  choice
    [ 
      If <$> (symbol IfToken *> pCondition)
         <*> (symbol LBraceToken *> pExpr <* symbol RBraceToken)
         <*> optional pElseOrElseIf
    , 
      symbol LBraceToken *> pExpr <* symbol RBraceToken
    ]

-- Parse a while expression
pWhile :: Parser Expr
pWhile = While <$> (symbol WhileToken *> pCondition)
               <*> (symbol LBraceToken *> pExpr <* symbol RBraceToken)

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
pTerm = choice
  [ pParens
  , pIf
  , pWhile
  , pReturn
  , pPrintLn
  , pVariable
  , pInteger
  ]

-- Parse an expression
pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

-- TABLE
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix SubtractToken Negative ]
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

commaType :: Token -> (Type -> Type -> Type) -> Operator Parser Type
commaType tok f = InfixL (f <$ symbol tok)

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
