module Tokenizer.Tokenizer
    ( someFunc
    , tokenize 
    ) where

import Data.Char
import Text.Read


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
             | IntegerToken Int| IdentifierToken String | StructNameToken String |SelfTypeToken | TraitToken | StructToken | ForToken
             | NewToken
                deriving (Show, Eq,Read)


-- Uses words to split a string into a list of strings. Pattern matching to deal with emtpy strings. 
-- Unsure of how to use this to put into a list so we can actually go through and tokenize it.
stripWhiteSpace :: String -> [String]
stripWhiteSpace [] = []
stripWhiteSpace x = words x

-- function removes white space up to character
removeLeadingWhiteSpace :: String -> String
removeLeadingWhiteSpace xs = dropWhile isSpace xs



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
tryReadIdentifierOrReservedWord "new" = NewToken
tryReadIdentifierOrReservedWord "trait" = TraitToken
tryReadIdentifierOrReservedWord "struct" = StructToken
tryReadIdentifierOrReservedWord "for" = ForToken
tryReadIdentifierOrReservedWord x = IdentifierToken x


tryReadIntegerToken :: String -> Token
tryReadIntegerToken x = IntegerToken (read x :: Int)

tryReadMultiCharSymbol :: String -> Maybe (Token, String)
tryReadMultiCharSymbol ('=' : '>' : xs) = Just (ArrowToken, xs)
tryReadMultiCharSymbol ('=' : '=' : xs) = Just (EqualsToken, xs)
tryReadMultiCharSymbol ('!' : '=' : xs) = Just (NotEqualToken, xs)
tryReadMultiCharSymbol _ = Nothing

tryReadSymbolToken :: Char -> Maybe Token 
tryReadSymbolToken '=' = Just EqualToken
tryReadSymbolToken '>' = Just GreaterThanToken
tryReadSymbolToken '<' = Just LessThanToken
tryReadSymbolToken '+' = Just AddToken
tryReadSymbolToken '-' = Just SubtractToken  
tryReadSymbolToken '*' = Just MultiplyToken
tryReadSymbolToken '/' = Just DivideToken
tryReadSymbolToken '(' = Just LParenToken
tryReadSymbolToken ')' = Just RParenToken
tryReadSymbolToken '{' = Just LBraceToken
tryReadSymbolToken '}' = Just RBraceToken
tryReadSymbolToken ',' = Just CommaToken
tryReadSymbolToken ':' = Just ColonToken
tryReadSymbolToken ';' = Just SemiColonToken
tryReadSymbolToken _ = Nothing


--entry point for tokenization
tokenize :: String -> Either String [Token]
tokenize input = tokenizationLoop (removeLeadingWhiteSpace input)

-- recursively loop extracting tokens
tokenizationLoop :: String -> Either String [Token]
-- base case when all characters have been tokenized
tokenizationLoop "" = Right []

--recursive case
tokenizationLoop s = 
  case nextToken s of
    Left err -> Left err
    Right (currToken, restStringList) -> do
      let stringWithNoSpaceInHeader = removeLeadingWhiteSpace restStringList
      currTokenList <- tokenizationLoop stringWithNoSpaceInHeader
      return (currToken : currTokenList)

-- extract a single token from list
nextToken :: String -> Either String (Token, String)
nextToken "" = Left "Input unexpetedly ended"

nextToken (firstChar:restOfInputString)
  | isAlpha firstChar = handleIdentifierOrReserveWord firstChar restOfInputString
  | isDigit firstChar = handleNumber firstChar restOfInputString
  | otherwise = handleSymbol (firstChar:restOfInputString)

-- Function that creates identifiers and reserved words tokens
handleIdentifierOrReserveWord :: Char -> String -> Either String (Token, String)
handleIdentifierOrReserveWord firstChar restOfInputString =
  let (identityBody, leftoverString) = span isAlphaNum restOfInputString
    identifier = firstChar : identityBody
    newToken = tryReadIdentifierOrReservedWord identifier
  in Right (newToken, leftoverString)


-- Function that creates Number token from Input String
handleNumber :: Char -> String -> Either String (Token, String)
handleNumber firstNum restOfInputString =
  let (numbers, leftoverString) = span isDigit restOfInputString
      wholeNumber = firstNum : numbers
  in case readMaybe wholeNumber of
       Just intValue -> Right (IntegerToken intValue, leftoverString)
       Nothing       -> Left $ "Invalid integer: " ++ wholeNumber


-- Function that creates Symbol Token from Input String
handleSymbol :: String -> Either String (Token, String)
handleSymbol [] = Left "No input to match symbol."

handleSymbol wholeString@(firstChar:restOfInputString) =
  case tryReadMultiCharSymbol wholeString of
    Just (newToken, leftoverString) -> Right (newToken, leftoverString)
    Nothing ->
      case tryReadSymbolToken firstChar of
        Just newToken -> Right (newToken, restOfInputString)
        Nothing -> Left $ "Unrecognized symbol near here `: " ++ take 8 wholeString ++ "`"




                          
main :: IO ()
main = do
  putStrLn "Enter input to tokenize:"
  input <- getLine
  case tokenize input of
    Left err -> putStrLn $ "Error: " ++ err
    Right tokens -> do
      putStrLn "Tokens:"
      print tokens

