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


-- Structdef parser
-- structdef ::= `struct` structname `{` comma_param `}`
parseStructDef :: Parser StructDef
parseStructDef =
  StructDef
    <$ checkMatchingToken StructToken
    -- Hold it as a StructName type within the Grammar, go back to this, may need to be changed
    <*> isIdentifierToken
    <* checkMatchingToken LBraceToken
    -- THIS PARSER STILL NEEDS TO BE DEFINED
    <*> parseCommaParam
    <* checkMatchingToken RBraceToken

-- ImplDef Parser
-- impldef ::= `impl` traitname `for` type `{` conc_methoddef* `}`
parseImplDef :: Parser ImplDef
parseImplDef =
  ImplDef
    <$ checkMatchingToken ImplToken
    <*> isIdentifierToken
    <* checkMatchingToken ForToken
    --THIS IS JUST PLACEHOLDER UNTIL type Parser function is brought in
    <*> parseTypeDef
    <* checkMatchingToken LBraceToken
    -- THIS PARSER STILL NEEDS TO BE DEFINED
    <*> many parseConcMethodDef
    <* checkMatchingToken RBraceToken

-- FuncDef Parser
-- funcdef ::= `func` var `(` comma_param `)` `:` type `{` stmt* `}`
parseFuncDef :: Parser FuncDef
parseFuncDef =
  FuncDef
    --REMIND myself to make FuncToken 
    <$ checkMatchingToken FuncToken
    --Probably want to be saved as VariableType?
    <*> IdentifierToken
    <* checkMatchingToken LParenToken
    <*> parseCommaParam
    <* checkMatchingToken RParenToken
    <* checkMatchingToken ColonToken
    -- PLACEHOLDER until Parse type checker is imported
    <*> parseTypeDef
    <* checkMatchingToken LBraceToken
    -- PLACEHOLDER until parseStatement is brought in
    <*> many parseStatment
    <* checkMatchingToken RBraceToken





