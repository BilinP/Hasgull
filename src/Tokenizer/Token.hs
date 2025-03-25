module Tokenizer.Token (
   Token(..)
)where


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
         | MethodToken 
         | BreakToken 
         | ImplToken 
         | LetToken
         | TraitToken
         | NewToken
         | StructToken
         | IntegerToken Int
         | IdentifierToken String 
         | StructNameToken String
          deriving (Show, Eq,Read)