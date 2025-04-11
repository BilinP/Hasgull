module Parser.Parser (
   parseExpression
) where

import Tokenizer.Token (Token(..))
import Control.Monad.Combinators.Expr
import Control.Applicative
import Text.Megaparsec hiding (Token) -- Hide Token to avoid ambuiguity.
import Text.Megaparsec.Char
import Text.Megaparsec.Char()
import Data.Void

type Parsec = Parsec Void [Token]

data Expr 
   =  Identifier String
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
   deriving (Eq,Ord,Show)

data Type
    = IntType
    | VoidType
    | BooleanType
    | SelfType
    | StructName String
    | CommaType --Unsure of this
    | HighOrderType
    deriving (Eq,Ord,Show)

data StructActualParam
   = StructParam String Expr
   deriving(Eq, Ord, Show)

--parse a variable
pVariable :: Parser Expr
pVariable = Identifier <$> (satisfy isIdentifierToken >>= \(IdentifierToken name) -> pure name)

-- Helper
isIdentifierToken :: Token -> Bool
isIdentifierToken (IdentifierToken _) = True
isIdentifierToken _ = False


--parse an integer
pInteger :: Parser Expr
pInteger  = Integer <$> (satisfy isIntegerToken >>= \(IntegerToken value) -> pure value)


-- Helper to check if Token is an Integertoken
isIntegerToken :: Token -> Bool
isIntegerToken (IntegerToken _) = True
isIntegerToken _ = False


pAtom :: Parser Expr
pAtom = choice [pParensAtom, pVariable, pInteger, pBoolean]

pParensAtom :: Parser Expr
pParensAtom = between (symbol LParenToken) (symbol RParenToken) pAtom

pReturn :: Parser Expr
pReturn = Return <$> (symbol ReturnToken *> pAtom)

pPrintLn :: Parser Expr
pPrintLn = PrintLn <$> (symbol PrintLnToken *> pAtom)


--Parse types
pType :: Parser Type
pType = (IntType <$ symbol IntToken) <|> (VoidType <$ symbol VoidToken) <|> (BooleanType <$ symbol BooleanToken)
         <|> (SelfType <$ symbol SelfToken)
         <!> (StructName <$> (satisfy isIdentifierToken >>= \(IdentifierToken  name)) -> pure name)
         <!> (between (symbol LParenToken ) (symbol RParenToken ) pType)
         <!> pHigherOrderType --Unsure of this

--Weird parser attempt for the ',' type part of commaType in the grammar
pCommaType :: Parser Type
pCommaType =
        CommaType =
        <* checkMatchingToken CommaToken 
        <*> pType
        
--Separate parser for the higher order type 
pHigherOrderType :: Parser Type
pHigherOrderType = 
        HigherType =
                <* checkMatchingToken LParenToken
                <*> pType
                <*> many pCommaType
                <* checkMatchingToken RParenToken
                <* checkMatchingToken ArrowToken
                <*> pType

-- Parse Struct Actual Params
pStructActParams :: Parser StructActualParam


-- Helper by Angel. Should remove to pull Angel's version.
checkMatchingToken :: Token -> Parser Token
checkMatchingToken t = label (show t) $ satisfy (== t)

-- Parse Boolean Literal
pBoolean :: Parser Expr
pBoolean = (Identifier "true" <$ symbol TrueToken) <|> (Identifier "false" <$ symbol FalseToken)

pParens :: Parser Expr
pParens = between (symbol LParenToken) (symbol RParenToken) pExpr

pIf :: Parser Expr
pIf = If <$> (symbol IfToken *> pCondition)
      <*> (symbol LBraceToken *> pExpr <* symbol RBraceToken)
      <*> optional pElseOrElseIf

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

pWhile :: Parser Expr
pWhile = While <$> (symbol WhileToken *> pCondition)
               <*> (symbol LBraceToken *> pExpr <* RBraceToken)


pCondition :: Parser Expr
pCondition = makeExprParser condTerm condOperatorTable

pParenCondition :: Parser Expr
pParenCondition = between (symbol LParenToken) (symbol RParenToken) pCondition

condTerm :: Parser Expr
condTerm = choice
[
   pBoolean
,  pVariable
,  pInteger
,  pParenCondition
]

condOperatorTable :: [[Operator Parser Expr]]
condOperatorTable =
        [ [
             binary EqualsToken Equals
          ,  binary NotEqualToken NotEquals
          , binary GreaterThanToken GreaterThan
          , binary LessThanToken LessThan
          ]
        ]

pTerm :: Parser Expr
pTerm = choice 
[
   pParen,
   pIf,
   pWhile,
   pReturn,
   pPrintLn,
   pExpr,
   pInteger

]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable = 
        [ [prefix SubtractToken Negative ]
        , [ binary MultiplyToken Multiply
          , binary DivideToken Division
          ]
        , [ binary AddToken Add
          , binary SubtractToken Sub
          ]
        , [
            binary EqualsToken Equals
          , binary NotEqualToken NotEquals
          , binary GreaterThanToken GreaterThan
          , binary LessThanToken LessThan
        ]
        ]


binary :: Token (Expr -> Expr -> Expr ) -> Operator Parser Expr
binary tok f = InfixL(f <$ symbol tok)

prefix :: Token -> (Expr -> Expr) -> Operator Parser Expr
prefix tok f = Prefix (f <$ symbol tok)

symbol :: Token -> Parser Token
symbol t = satisfy (== t)


parseExpression :: [Token] -> Either (ParseErrorBundle [Token] Void) Expr
parseExpression = runParser pExpr ""


  

        

