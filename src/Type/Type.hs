
{-
module Type.Type
    ( Token
    ) where


  data Token =
             EQUALSEQUALS | EQUALS | NOT_EQUALS | GREATER_THAN | LESS_THAN |          --Symbols
             ADD | SUBTRACT | MULTIPLY | DIVIDE | LPAREN | RPAREN | LBRACE | RBRACE |   
             COMMA | COLON | ARROW | SEMICOLON |
             IntToken | VoidToken | BooleanToken | IfToken | ElseToken | WhileToken | ReturnToken  --Reserved words
             | PrintLnToken | TrueToken | FalseToken | SelfToken | MethodToken | BreakToken | ImplToken
             | IntegerToken Int| IdentifierToken String | StructNameToken String
                deriving (Show, Eq)


someFunc :: IO ()
someFunc = putStrLn "someFunc"

-}


-- Date : 03/05/2025
-- Explaination: The below text is me ordering out our tokens for so we have a guideline to follow for construction of our tokenizer.
-- This isn't conclusive, so if you spot one that I missed, please put it in .
-- The formating goes as : TokenName :: what is in the grammer on the Hasgull document.
--  Consideration of what our tokens are.
--  Symbols
--
-- Starting with symbols because that's easier to define.
--
--  EQUALSEQUALS :: `==`
--  EQUALS :: `=`
--  NOT_EQUALS :: `!=`
--  GREATER_THAN :: `>`
--  LESS_THAN :: `<`
--  ADD :: `+`
--  SUBTRACT :: `-`
--  MULTIPLY :: `*`
--  DIVIDE :: `/`
--  LPAREN :: `(`
-- RPAREN :: `)`
-- LBRACE :: `{`
-- RBRACE :: `}`
-- COMMA :: `,`
-- COLON :: `:`
-- ARROW :: `=>`
-- SEMICOLON :: `;`


--  Reserved Words
-- in relation with our grammer, this is any word that is within `` ticks or does not represent a rule.
-- Int :: `Int`
-- Void :: `Void`
-- Boolean :: `Boolean`
--  if :: `if`
--  else :: `else`
--  while :: `while`
--  return :: `return`
--  printLn :: `println`
--  true :: `true`
--  false :: `false`
-- self :: `self`
-- method :: `method`
-- break :: `break`
-- impl :: `impl`


-- Identifiers
-- Identifiers are any word that is not a reserved word or a symbol.
-- Can this github copilot fuck off
-- Integer(int) :: i
-- Identifier(String) :: var
-- StructName(String) :: structname