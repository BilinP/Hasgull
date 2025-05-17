{-|
Module      : Tokenizer.Token
Description : Defines tokens used by the Hasgull language tokenizer.

This module defines the Token data type and its constructors which represent the
different tokens recognized by the tokenizer of the Hasgull programming language.
-}
module Tokenizer.Token (
   Token(..)
) where

{-|
The 'Token' data type defines all tokens recognized by the Hasgull language.
These tokens are used during lexical analysis to represent keywords,
operators, punctuation, and other language elements.
-}
data Token =
         EqualsToken       -- ^ Represents the '==' operator.
         | EqualToken      -- ^ Represents the '=' operator.
         | NotEqualToken   -- ^ Represents the '!=' operator.
         | GreaterThanToken -- ^ Represents the '>' operator.
         | LessThanToken    -- ^ Represents the '<' operator.
         | AddToken         -- ^ Represents the '+' operator.
         | SubtractToken    -- ^ Represents the '-' operator.
         | MultiplyToken    -- ^ Represents the '*' operator.
         | DivideToken      -- ^ Represents the '/' operator.
         | LParenToken      -- ^ Represents the '('.
         | RParenToken      -- ^ Represents the ')'.
         | LBraceToken      -- ^ Represents the '{'.
         | RBraceToken      -- ^ Represents the '}'.
         | CommaToken       -- ^ Represents the ','.
         | ColonToken       -- ^ Represents the ':'.
         | ArrowToken       -- ^ Represents the '=>/' operator.
         | SemiColonToken   -- ^ Represents the ';'.
         | DotToken         -- ^ Represents the '.'.
         | IntToken         -- ^ Represents the 'Int' keyword.
         | VoidToken        -- ^ Represents the 'Void' keyword.
         | BooleanToken     -- ^ Represents the 'Boolean' keyword.
         | IfToken          -- ^ Represents the 'if' keyword.
         | ElseToken        -- ^ Represents the 'else' keyword.
         | WhileToken       -- ^ Represents the 'while' keyword.
         | ReturnToken      -- ^ Represents the 'return' keyword.
         | PrintLnToken     -- ^ Represents the 'println' keyword.
         | FuncToken        -- ^ Represents the 'func' keyword.
         | TrueToken        -- ^ Represents the 'true' literal.
         | FalseToken       -- ^ Represents the 'false' literal.
         | SelfToken        -- ^ Represents the 'Self' keyword.
         | LowerCaseSelfToken -- ^ Represents the 'self' keyword.
         | MethodToken      -- ^ Represents the 'method' keyword.
         | BreakToken       -- ^ Represents the 'break' keyword.
         | ImplToken        -- ^ Represents the 'impl' keyword.
         | LetToken         -- ^ Represents the 'let' keyword.
         | TraitToken       -- ^ Represents the 'trait' keyword.
         | NewToken         -- ^ Represents the 'new' keyword.
         | ForToken         -- ^ Represents the 'for' keyword.
         | StructToken      -- ^ Represents the 'struct' keyword.
         | IntegerToken Int -- ^ Represents an integer literal.
         | IdentifierToken String -- ^ Represents an identifier.
         | StructNameToken String -- ^ Represents the name of a structure.
          deriving (Eq, Ord, Show)