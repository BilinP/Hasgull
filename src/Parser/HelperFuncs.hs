module Parser.Helpers (checkMatchingToken) where

import Text.Megaparsec
import Data.Void (Void)
import Tokenizer.Token (Token(..))
import qualified Tokenizer.Token as T

type Parser = Parsec Void [Token]

-- | Match exactly the given token, consuming one token from the stream.
checkMatchingToken :: Token -> Parser Token
checkMatchingToken t = label (show t) $ satisfy (== t)

-- Parse a variable
pVariable :: Parser Expr
pVariable = Identifier <$> (satisfy isIdentifierToken >>= \(IdentifierToken name) -> pure name)

isIdentifierToken :: Token -> Bool
isIdentifierToken (IdentifierToken _) = True
isIdentifierToken _ = False

pIdentifier :: Parser String
pIdentifier = do
  tok <- satisfy isIdentifierToken
  case tok of
    IdentifierToken name -> pure name
    _ -> fail "Expected identifier"
