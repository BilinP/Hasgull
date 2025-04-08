module Parser.Expr(
   Expr(..)
)where

import Tokenizer.Token (Token(..));

    
    data Program = 
         ProgramItem | Stmt


    data ProgramItem =
         StructDef | TraitDef | Impldef | FuncDef


    data Exp = 
         EqualsExp

    data EqualsExp =
          LessThanExp EqualsToken LessThanExp | LessThanExp NotEqualToken LessThanExp 

    data LessThanExp =
          AddExp LessThanToken AddToken

    data AddExp =
          MultExp AddToken MultExp | MultExp SubtractToken MultExp
    
    data MultExp =
           CallExp MultiplyToken CallExp | CallExp DivideToken CallExp

    data CallExp = 
            DotExp LParenToken CommaExp RParenToken 
    
    data DotExp =
             PrimaryExp DotToken var

    data PrimaryExp =
             IntegerToken Int  | IdentifierToken String |
             TrueToken | FalseToken | LowerCaseSelfToken 
             | LParenToken Exp RParenToken | NewToken StructName LBraceToken StructActualParams RBraceToken

    data StructActualParams = 
         StructActualParam CommaToken StructActualParam

    data StructActualParam =
        IdentifierToken String ColonToken Exp


    data Stmt =
         LetToken Param EqualToken Exp SemiColonToken |
         IdentifierToken String EqualToken Exp SemiColonToken |
         IfToken LParenToken Exp RParenToken Stmt ElseToken Stmt |
         WhileToken LParenToken Exp RParenToken Stmt |
         BreakToken SemiColonToken |
         PrintLnToken LParenToken Exp RParenToken |
         LBraceToken Stmt RBraceToken |
         ReturnToken Exp SemiColonToken |
         Exp SemiColonToken

    data FuncDef =
        FuncDef IdentifierToken String LParenToken CommmaParam RParenToken ColonToken Type -- Da hell?

    data ImplDef =
        ImplToken TraitName ForToken Type LBraceToken ConcMethodDef RParenToken

    data TraitDef =
         TraitToken TraitName LBraceToken AbsMethodDef RBraceToken
    
    data ConcMethodDef  =
        MethodToken IdentifierToken String LParenToken CommmaParam RParenToken ColonToken Type LBraceToken Stmt RBraceToken

    data AbsMethodDef =
        MethodToken IdentifierToken String LParenToken CommaParam RParenToken ColonToken Type SemiColonToken

    data StructDef =
        StructToken  StructName LBraceToken COmmaParam RBraceToken

    data CommaParam = 
        Param CommaToken Param

    data Param =
        IdentifierToken String ColonToken Type

    data Type =
        IntToken | VoidToken | Boolean |
        SelfToken |
        StructNameToken String |
        LParenToken Type RParenToken |
        LParenToken CommaType RParenToken ArrowToken Type
        
    data CommaType =
        Type CommaToken Type
   


   