module Parser.Parser
    ( someFunc
    ) where

import Text.Megaparsec
import Data.Void
import AST
import Tokenizer.Token
import Parser.Helpers


-- erase if someone already made this
type Parser = Parsec Void [Token]
-- erase if someone already made this



-- Trait Parser
parseTraitDf :: Parser TraitDef
parseTraitDef =
  TraitDef
    <$ checkMatchingToken Trait
    <*> isIdentifierToken 
    <* checkMatchingToken LBraceToken
    <*> many parseAbsMethodDef
    <* symbol RBraceToken

-- Abstract method Definition Parser
parseAbsMethodDef :: Parser AbsMethodDef
parseAbsMethodDef =
  AbsMethodDef
    <$ checkMatchingToken MethodToken
    <*> isIdentifierToken 
    <* checkMatchingToken LParenToken
    <*> parseCommaParam
    <* checkMatchingToken RParenToken
    <* checkMatchingToken ColonToken
    --THIS IS JUST PLACEHOLDER UNTIL type Parser  function is brought in
    <*> parseTypeDef
    <* checkMatchingToken SemiColonToken