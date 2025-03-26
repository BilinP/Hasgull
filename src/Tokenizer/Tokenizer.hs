
module Tokenizer.Tokenizer (
  tokenize,
) where

import Data.Char
import Text.Read
import Tokenizer.Token (Token(..))



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
tryReadSymbolToken ';' = Just SemiColonToken
tryReadSymbolToken _ = Nothing

-- | The main tokenizing function
tokenize :: String -> Either String [Token]
tokenize input = tokenizationLoop (removeLeadingWhiteSpace input)

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
        Just intValue -> Right (IntegerToken intValue, leftoverString)
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

-- Example main (usually you'd put this in Main.hs, but here for demo):
main :: IO ()
main = do
  putStrLn "Enter input to tokenize:"
  input <- getLine
  case tokenize input of
    Left err -> putStrLn $ "Error: " ++ err
    Right toks -> do
      putStrLn "Tokens:"
      print toks