{-|
Module      : Tokenizer.Tokenizer
Description : A simple tokenizer for the Hasgull language.

This module provides functionality to tokenize input source code by removing
comments and whitespace, then converting the input into a list of tokens as defined in
'Tokenizer.Token'.
-}
module Tokenizer.Tokenizer (
  tokenize,
  stripWhiteSpace,
  removeComments,
  removeLeadingWhiteSpace,
  tryReadIdentifierOrReservedWord,
  tryReadMultiCharSymbol,
  tryReadSymbolToken,validLeftOver,tokenizeStrippedWords,tokenizationLoop,nextToken,handleIdentifierOrReserveWord,handleNumber,handleSymbol
) where

import Data.Char
import Text.Read
import Tokenizer.Token (Token(..))

{-|
Remove line comments from the input.

Cool gimmick we wanted to include in our compiler to be able add comments to our code.
Line comments begin with "//" and continue until the end of the line.
-}
removeComments :: String -> String
removeComments [] = []
removeComments ('/' : '/' : rest) = removeComments (dropWhile (/= '\n') rest)
removeComments (keep : check) = keep : removeComments check 

{-|
Remove leading whitespace from a string.

Drops any space characters ('isSpace') from the beginning of the input.
-}
removeLeadingWhiteSpace :: String -> String
removeLeadingWhiteSpace = dropWhile isSpace

{-|
Determine if an identifier string corresponds to a reserved word, or should be treated
as a generic identifier.

Recognized reserved words include "Int", "Void", "if", "else", etc. Any identifier not
matching these is returned as an IdentifierToken.
-}
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
tryReadIdentifierOrReservedWord "Self" = SelfToken
tryReadIdentifierOrReservedWord "self" = LowerCaseSelfToken
tryReadIdentifierOrReservedWord "func" = FuncToken
tryReadIdentifierOrReservedWord "method" = MethodToken
tryReadIdentifierOrReservedWord "break" = BreakToken
tryReadIdentifierOrReservedWord "impl" = ImplToken
tryReadIdentifierOrReservedWord "let" = LetToken
tryReadIdentifierOrReservedWord "new" = NewToken
tryReadIdentifierOrReservedWord "trait" = TraitToken
tryReadIdentifierOrReservedWord "struct" = StructToken
tryReadIdentifierOrReservedWord "for" = ForToken
tryReadIdentifierOrReservedWord x = IdentifierToken x

{-|
Attempt to read a multi-character symbol from the input.

Recognized multi-character symbols include:
  * "=>"  becomes ArrowToken
  * "=="  becomes EqualsToken
  * "!="  becomes NotEqualToken

Returns a tuple of the matching token and the remaining input string, or Nothing if no match.
-}
tryReadMultiCharSymbol :: String -> Maybe (Token, String)
tryReadMultiCharSymbol ('=' : '>' : xs) = Just (ArrowToken, xs)
tryReadMultiCharSymbol ('=' : '=' : xs) = Just (EqualsToken, xs)
tryReadMultiCharSymbol ('!' : '=' : xs) = Just (NotEqualToken, xs)
tryReadMultiCharSymbol _ = Nothing

{-|
Attempt to read a symbol token from a single character.

Converts characters such as '=' and '+' into their corresponding token variants.
Returns Nothing if the character is not a recognized symbol.
-}
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
tryReadSymbolToken '.' = Just DotToken
tryReadSymbolToken ';' = Just SemiColonToken
tryReadSymbolToken _ = Nothing

{-|
Strip whitespace from the input string and return a list of words.

Uses the built-in 'words' function to split the input on whitespace.
-}
stripWhiteSpace :: String -> [String]
stripWhiteSpace "" = []
stripWhiteSpace input = words input

{-|
Determine if the remaining part of a numerical literal is valid.

A leftover is invalid if it starts with an alphabetic character, indicating an error.
-}
validLeftOver :: String -> Bool 
validLeftOver "" = True
validLeftOver (x : xs)
   | isAlpha x = False
   | otherwise = True
     
{-|
Tokenize the input string into a list of tokens.

This function removes comments and whitespace before invoking the tokenization
of individual components.
-}
tokenize :: String -> Either String [Token]
tokenize input = 
     let strippedInput = stripWhiteSpace (removeComments input)
     in tokenizeStrippedWords strippedInput

{-|
Tokenize a list of whitespace-stripped words into tokens.

Processes each word and concatenates the results.
-}
tokenizeStrippedWords :: [String] -> Either String [Token]
tokenizeStrippedWords [] =  Right []  
tokenizeStrippedWords (word:rest) = do 
    tokensfromCurrent <- tokenizationLoop word
    tokensfromRest <- tokenizeStrippedWords rest
    return (tokensfromCurrent ++ tokensfromRest)

{-|
Recursively tokenizes a single string into a list of tokens.

Continues processing until the string is fully consumed.
-}
tokenizationLoop :: String -> Either String [Token]
tokenizationLoop "" = Right []
tokenizationLoop s =
  case nextToken s of
    Left err -> Left err
    Right (currToken, restStringList) -> do
      let stringWithNoSpaceInHeader = removeLeadingWhiteSpace restStringList
      moreTokens <- tokenizationLoop stringWithNoSpaceInHeader
      return (currToken : moreTokens)

{-|
Extract the next token from the input string.

Selects the correct handler based on the first character, whether it's alphabetic, numeric, or a symbol.
-}
nextToken :: String -> Either String (Token, String)
nextToken "" = Left "Input unexpectedly ended"
nextToken (firstChar : restOfInputString)
  | isAlpha firstChar = handleIdentifierOrReserveWord firstChar restOfInputString
  | isDigit firstChar = handleNumber firstChar restOfInputString
  | otherwise = handleSymbol (firstChar : restOfInputString)

{-|
Handle an identifier or reserved word.

Collects alphanumeric characters and returns a token corresponding to either a reserved word or an identifier.
-}
handleIdentifierOrReserveWord :: Char -> String -> Either String (Token, String)
handleIdentifierOrReserveWord firstChar restOfInputString =
  let (identityBody, leftoverString) = span isAlphaNum restOfInputString
      identifier = firstChar : identityBody
      newToken = tryReadIdentifierOrReservedWord identifier
   in Right (newToken, leftoverString)

{-|
Handle a numeric literal from the input.

Reads consecutive digits and checks for invalid trailing characters.
-}
handleNumber :: Char -> String -> Either String (Token, String)
handleNumber firstNum restOfInputString =
  let (numbers, leftoverString) = span isDigit restOfInputString
      wholeNumber = firstNum : numbers
   in case readMaybe wholeNumber of
        Just intValue ->
         if validLeftOver leftoverString
            then Right (IntegerToken intValue, leftoverString)
            else Left ("Invalid integer: " ++ wholeNumber ++ leftoverString)      
        Nothing -> Left ("Invalid integer: " ++ wholeNumber)

{-|
Handle symbols from the input string.

First attempts to match multi-character symbols, then falls back to single-character symbol matching.
-}
handleSymbol :: String -> Either String (Token, String)
handleSymbol [] = Left "No input to match symbol."
handleSymbol wholeString@(firstChar : restOfInputString) =
  case tryReadMultiCharSymbol wholeString of
    Just (newToken, leftoverString) -> Right (newToken, leftoverString)
    Nothing ->
      case tryReadSymbolToken firstChar of
        Just newToken -> Right (newToken, restOfInputString)
        Nothing ->
          Left ("Unrecognized symbol near here `: " ++ take 8 wholeString ++ "`")