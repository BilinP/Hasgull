
module Parser.Parser (
  Expr(..),
  parseExpression
) where

import Tokenizer.Token (Token(..))
import Control.Monad.Combinators.Expr
import Text.Megaparsec hiding (Token) -- Hide Token to avoid ambiguity
import Text.Megaparsec.Char
import Data.Void

-- Define the parser type
type Parser = Parsec Void [Token]

-- Define the expression data type
data Expr
  = Var String
  | Int Int
  | Negation Expr
  | Sum Expr Expr
  | Sub Expr Expr
  | Product Expr Expr
  | Division Expr Expr
  deriving (Eq, Ord, Show)

-- Parse a variable
pVariable :: Parser Expr
pVariable = do
  token <- satisfy isIdentifierToken
  case token of
    IdentifierToken name -> return (Var name)
    _ -> fail "Expected an identifier"

-- Parse an integer
pInteger :: Parser Expr
pInteger = do
  token <- satisfy isIntegerToken
  case token of
    IntegerToken value -> return (Int value)
    _ -> fail "Expected an integer"

-- Helper to check if a token is an identifier
isIdentifierToken :: Token -> Bool
isIdentifierToken (IdentifierToken _) = True
isIdentifierToken _ = False

-- Helper to check if a token is an integer
isIntegerToken :: Token -> Bool
isIntegerToken (IntegerToken _) = True
isIntegerToken _ = False

-- Parse parentheses
pParens :: Parser Expr
pParens = between (symbol LParenToken) (symbol RParenToken) pExpr

-- Parse a term
pTerm :: Parser Expr
pTerm = choice
  [ pParens
  , pVariable
  , pInteger
  ]

-- Parse an expression
pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

-- Define the operator table
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix SubtractToken Negation ]
  , [ binary MultiplyToken Product
    , binary DivideToken Division
    ]
  , [ binary AddToken Sum
    , binary SubtractToken Sub
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


