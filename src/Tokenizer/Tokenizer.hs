module Tokenizer.Tokenizer
    ( someFunc
    ) where

import Data.Char


-- Goal: Take a string and return a list of tokens through our Tokenizer.
-- 1. Take a string and split it into words.  We can do this by calling the default words function on our input string. done!
-- 2. Go through our list of words and determine if it matches any of our tokens. If it does, append it to oru list of tokens done!
-- 3. Create exceptions handling for invalid data.
-- 4. Create test cases to determine coverage

    

data Token =
             EqualsToken | EqualToken | NotEqualToken | GreaterThanToken | LessThanToken |          --Symbols
             AddToken | SubtractToken| MultiplyToken | DivideToken | LParenToken | RParenToken | LBraceToken | RBraceToken |   
             CommaToken | ColonToken | ArrowToken | SemiColonToken |
             IntToken | VoidToken | BooleanToken | IfToken | ElseToken | WhileToken | ReturnToken  --Reserved words
             | PrintLnToken | TrueToken | FalseToken | SelfToken | MethodToken | BreakToken | ImplToken | LetToken
             | IntegerToken Int| IdentifierToken String | StructNameToken String
                deriving (Show, Eq,Read)


-- Uses words to split a string into a list of strings. Pattern matching to deal with emtpy strings. 
-- Unsure of how to use this to put into a list so we can actually go through and tokenize it.
stripWhiteSpace :: String -> [String]
stripWhiteSpace [] = []
stripWhiteSpace x = words x





--Function that takes a string and returns the associated token.
-- Uses pattern matching
tryReadIdentifierOrReservedWord :: String -> Token
tryReadIdentifierOrReservedWord "Int" = IntToken
tryReadIdentifierOrReservedWord "Void" = VoidToken
tryReadIdentifierOrReservedWord "Boolean" = BooleanToken
tryReadIdentifierOrReservedWord "if" = IfToken
tryReadIdentifierOrReservedWord "else" = ElseToken
tryReadIdentifierOrReservedWord "while" = WhileToken
tryReadIdentifierOrReservedWord "return" = ReturnToken
tryReadIdentifierOrReservedWord "println" = PrintLnToken
tryReadIdentifierOrReservedWord "true" = TrueToken
tryReadIdentifierOrReservedWord "false" = FalseToken
tryReadIdentifierOrReservedWord "self" = SelfToken
tryReadIdentifierOrReservedWord "method" = MethodToken
tryReadIdentifierOrReservedWord "break" = BreakToken
tryReadIdentifierOrReservedWord "impl" = ImplToken
tryReadIdentifierOrReservedWord "let" = LetToken
tryReadIdentifierOrReservedWord x = IdentifierToken x


tryReadIntegerToken :: String -> Token
tryReadIntegerToken x = IntegerToken (read x :: Int)

tryReadSymbolToken :: String -> Token 
tryReadSymbolToken "=" = EqualToken
tryReadSymbolToken "==" = EqualsToken
tryReadSymbolToken "!=" = NotEqualToken
tryReadSymbolToken ">" = GreaterThanToken
tryReadSymbolToken "<" = LessThanToken
tryReadSymbolToken "+" = AddToken
tryReadSymbolToken "-" = SubtractToken  
tryReadSymbolToken "*" = MultiplyToken
tryReadSymbolToken "/" = DivideToken
tryReadSymbolToken "(" = LParenToken
tryReadSymbolToken ")" = RParenToken
tryReadSymbolToken "{" = LBraceToken
tryReadSymbolToken "}" = RBraceToken
tryReadSymbolToken "," = CommaToken
tryReadSymbolToken ":" = ColonToken
tryReadSymbolToken "=>" = ArrowToken
tryReadSymbolToken ";" = SemiColonToken


-- Takes a string and matches against our tokenizing functions to return the matching token
convertToToken testInput 
          | isAlpha (head testInput) = tryReadIdentifierOrReservedWord testInput
          | isDigit (head testInput)  = tryReadIntegerToken testInput
          | isAscii (head testInput)  = tryReadSymbolToken testInput
          | otherwise = error "Invalid input"

symbolLast :: String -> [Token]
symbolLast input = [tryReadIdentifierOrReservedWord (init input),tryReadSymbolToken [last input]]
symbolFirst :: String -> [Token]
symbolFirst input = [tryReadSymbolToken [head input], tryReadIdentifierOrReservedWord (tail input)]

handleSymbolwIdentifer stringConvert
         | isAscii(head stringConvert) = symbolFirst stringConvert
         | isAscii(last stringConvert) = symbolLast stringConvert  
         | otherwise = error "Invalid"        
                  
-- takes a string and returns a list of the equivalent tokens
tokenizer :: String -> [Token]
tokenizer userInput = map convertToToken (stripWhiteSpace userInput)

                            

someFunc :: IO ()
someFunc = putStrLn "someFunc"
