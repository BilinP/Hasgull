module Tokenizer.Token (
   Token(..)
)where

   -- Moved Token definitions to here just to make a bit clearer where they are being defined. Mostly for Test cases.
    data Token =
         EqualsToken 
         | EqualToken 
         | NotEqualToken 
         | GreaterThanToken 
         | LessThanToken 
         | AddToken 
         | SubtractToken 
         | MultiplyToken 
         | DivideToken 
         | LParenToken 
         | RParenToken 
         | LBraceToken 
         | RBraceToken 
         | CommaToken 
         | ColonToken 
         | ArrowToken 
         | SemiColonToken 
         |IntToken 
         | VoidToken 
         | BooleanToken 
         | IfToken 
         | ElseToken 
         | WhileToken 
         | ReturnToken
         | PrintLnToken 
         | TrueToken 
         | FalseToken 
         | SelfToken 
         | LowerCaseSelfToken
         | MethodToken 
         | BreakToken 
         | ImplToken 
         | LetToken
         | TraitToken
         | NewToken
         | ForToken
         | StructToken
         | IntegerToken Int
         | IdentifierToken String 
         | StructNameToken String
          deriving (Show, Eq,Read)