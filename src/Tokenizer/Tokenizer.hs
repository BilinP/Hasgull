
module Tokenizer.Tokenizer (
  tokenize,
  stripWhiteSpace
) where

import Data.Char
import Text.Read
import Tokenizer.Token (Token(..))

removeComments :: String -> String
removeComments [] = []
removeComments ('/' : '/' : rest) = removeComments (dropWhile (/= '\n') rest)
removeComments (keep : check) = keep : removeComments check 

-- Removes leading whitespace from a string
removeLeadingWhiteSpace :: String -> String
removeLeadingWhiteSpace = dropWhile isSpace

-- Decide if a string is a known keyword or just an identifier
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
tryReadIdentifierOrReservedWord "method" = MethodToken
tryReadIdentifierOrReservedWord "break" = BreakToken
tryReadIdentifierOrReservedWord "impl" = ImplToken
tryReadIdentifierOrReservedWord "let" = LetToken
tryReadIdentifierOrReservedWord "new" = NewToken
tryReadIdentifierOrReservedWord "trait" = TraitToken
tryReadIdentifierOrReservedWord "struct" = StructToken
tryReadIdentifierOrReservedWord "for" = ForToken
tryReadIdentifierOrReservedWord x = IdentifierToken x


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
tryReadSymbolToken '.' = Just DotToken
tryReadSymbolToken ';' = Just SemiColonToken
tryReadSymbolToken _ = Nothing




stripWhiteSpace :: String -> [String]
stripWhiteSpace "" = []
stripWhiteSpace input = words input

--Determines if the left over string in the second list of span tuple is valid.
-- This should only really be used in handleNumber, as 123abc for example should not be a valid token
validLeftOver :: String -> Bool 
validLeftOver "" = True -- no more left over words, so automatically a valid integer
validLeftOver (x : xs) -- Take from the first character 
   | isAlpha x = False
   | otherwise = True
     
-- | The main tokenizing function
tokenize :: String -> Either String [Token]
tokenize input = 
     let strippedInput = stripWhiteSpace (removeComments input) --Remove comments before we strip all whitespace
     in tokenizeStrippedWords strippedInput -- Strip all whitespace and go through the "words" list
      
-- Simply just tokenization loop but we use it on each "word" and add tokens
-- It should just basically be the exact logic as tokenizationLoop but goes through
tokenizeStrippedWords :: [String] -> Either String [Token]
tokenizeStrippedWords [] =  Right [] --Nothing in input string, base case     
tokenizeStrippedWords (word:rest) = do 
    tokensfromCurrent <- tokenizationLoop word --Tokenize current "word"
    tokensfromRest <- tokenizeStrippedWords rest    -- Tokenize the rest of the word list
    return (tokensfromCurrent ++ tokensfromRest)


tokenizationLoop :: String -> Either String [Token]
-- base case
tokenizationLoop "" = Right []
-- recursive case
tokenizationLoop s =
  case nextToken s of
    Left err -> Left err
    Right (currToken, restStringList) -> do
      let stringWithNoSpaceInHeader = removeLeadingWhiteSpace restStringList
      moreTokens <- tokenizationLoop stringWithNoSpaceInHeader
      return (currToken : moreTokens)

-- Get a single token from the front
nextToken :: String -> Either String (Token, String)
nextToken "" = Left "Input unexpectedly ended"
nextToken (firstChar : restOfInputString)
  | isAlpha firstChar = handleIdentifierOrReserveWord firstChar restOfInputString
  | isDigit firstChar = handleNumber firstChar restOfInputString
  | otherwise = handleSymbol (firstChar : restOfInputString)

-- This is the function that had the parse-error due to indentation.
handleIdentifierOrReserveWord :: Char -> String -> Either String (Token, String)
handleIdentifierOrReserveWord firstChar restOfInputString =
  let (identityBody, leftoverString) = span isAlphaNum restOfInputString
      identifier = firstChar : identityBody
      newToken = tryReadIdentifierOrReservedWord identifier
   in Right (newToken, leftoverString)

handleNumber :: Char -> String -> Either String (Token, String)
handleNumber firstNum restOfInputString =
  let (numbers, leftoverString) = span isDigit restOfInputString
      wholeNumber = firstNum : numbers
   in case readMaybe wholeNumber of
        Just intValue ->
         if validLeftOver leftoverString -- if nothing within leftoverString or leftover doesn't start with a letter, valid integer.
            then Right (IntegerToken intValue, leftoverString)
            else Left ("Invalid integer: " ++ wholeNumber ++ leftoverString)      
        Nothing -> Left ("Invalid integer: " ++ wholeNumber)

handleSymbol :: String -> Either String (Token, String)
handleSymbol [] = Left "No input to match symbol."
handleSymbol wholeString@(firstChar : restOfInputString) =
  case tryReadMultiCharSymbol wholeString of
    Just (newToken, leftoverString) -> Right (newToken, leftoverString)
    Nothing ->
      case tryReadSymbolToken firstChar of
        Just newToken -> Right (newToken, restOfInputString)
        Nothing ->
          Left
            ( "Unrecognized symbol near here `: "
                ++ take 8 wholeString
                ++ "`"
            )