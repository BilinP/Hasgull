{
module Parser.Parser where
import Tokenizer.Tokenizer 
import Tokenizer.Token (Token(..))
}
%name parseTokens
%tokentype {Token}
%error { parseError}

%token                                --Match with our tokens so I don't have to put each TokenName everytime in the grammer.
   "=="       {EqualsToken}
    '='       {EqualToken}
    "!="      {NotEqualToken}
    '>'       {GreaterThanToken}
    '<'       {LessThanToken}
    '+'       {AddToken}
    '-'       {SubtractToken}
    '*'       {MultiplyToken}
    '/'       {DivideToken}
    '('       {LParenToken}
    ')'       {RParenToken}
    '{'       {LBraceToken}
    '}'       {RBraceToken}
    ','       {CommaToken}
    ':'       {ColonToken}
    "=>"      {ArrowToken}
    ';'       {SemiColonToken}
    '.'       {DotToken}
    Int       {IntToken}
    Void      {VoidToken}
    Boolean   {BooleanToken}
    if        {IfToken}
    else      {ElseToken}
    while     {WhileToken}
    return    {ReturnToken}
    println   {PrintLnToken}
    func      {FuncToken}
    true      {TrueToken}
    false     {FalseToken}
    Self      {SelfToken}
    self      {LowerCaseSelfToken}
    method    {MethodToken}
    break     {BreakToken}
    impl      {ImplToken}
    let       {LetToken}
    trait     {TraitToken}
    new       {NewToken}
    for       {ForToken}
    struct    {StructToken}
    i         {IntegerToken $$}
    var       {IdentifierToken $$}
    
%left '==' '!=='   
%left '<' '>'    
%left '+' '-'
%left '*' '/'
%nonassoc LOWER_THAN_ELSE
%nonassoc if else



%%

--Essentially just copied our grammer straight into here, mostly following the example on the Happy website
-- NOTE: the $1 essentially the value in the production rule, they must match.
Program : ProgramItems Stmts {Program $1 $2 }

ProgramItems : ProgramItem ProgramItems {$1 : $2}
             | {- empty -} {[]}

Stmts : Stmt Stmts {$1: $2 }
      | {- empty -} { [] }



ProgramItem : StructDef {StructDefLeaf $1}
            | TraitDef {TraitDefLeaf $1 }
            | ImplDef {ImplDefLeaf $1}
            | FuncDef {FuncDefLeaf $1}

Exp : EqualsExp { ExpRoot $1}


Exp : Exp '==' Exp { EqualsExpRoot $1 EqualsToken $3 }
    | Exp '!=' Exp { NotEqualsExpRoot $1 NotEqualToken $3 }
    | Exp '<' Exp { LessThanExpRoot $1 LessThanToken $3 }
    | Exp '>' Exp { LessThanExpRoot $1 GreaterThanToken $3 }
    | Exp '+' Exp { AddExpRoot $1 AddToken $3 }
    | Exp '-' Exp { AddExpRoot $1 SubtractToken $3 }
    | Exp '*' Exp { MultExpRoot $1 MultiplyToken $3 }
    | Exp '/' Exp { MultExpRoot $1 DivideToken $3 }
    | Exp '(' Exp ')' {CallExpRoot $1 $3}
    | Exp '.' var {DotExpRoot $1 $3}
    | '(' Exp ')' { $2 }
    | PrimaryExp { $1 }

PrimaryExp:  i {IntRoot $1}
            | var {VarRoot $1}
            | true {BoolRoot True}
            | false {BoolRoot False}
            | self {LowerSelfRoot}
            | '(' Exp ')' {ParenExpRoot $2 }
            | new var '{' StructActualParams '}' {NewRoot $2 $4 }

StructActualParams : StructActualParam ',' StructActualParams {$1 : $3}
                    | StructActualParam {[$1]}
                    | {- empty -} {[]}

StructActualParam : var ':' Exp  {StructActualParamRoot $1 $3}


Stmt : let Param '=' Exp ';' {LetStmt $2 $4}
       | var '=' Exp ';' {AssignStmt $1 $3}
       | if '(' Exp ')' Stmt else Stmt %prec LOWER_THAN_ELSE {IfStmt $3 $5 (Just $7)}
       | if '(' Exp ')' Stmt  {IfStmt $3 $5 Nothing}
       | while '(' Exp ')' Stmt {WhileStmt $3 $5}
       | break ';' {BreakStmt}
       | println '(' Exp ')' {PrintLnStmt $3}
       | '{' Stmts '}' {BlockStmt $2}
       | return Exp ';' {ReturnStmt (Just $2)}
       | return ';' {ReturnStmt Nothing}
       | Exp ';' {ExpStmt $1}

FuncDef :  func var '(' CommaParam ')' ':' Type '{' Stmts '}' {FuncDefRoot $2 $4 $7 $9}

ImplDef : impl var for Type '{' ConcMethodDefs '}' {ImplDefRoot $2 $4 $6}

TraitDef : trait var '{' AbsMethodDefs '}' {TraitDefRoot $2 $4} --It's var because I realized we don't actually have a way for there to be a traitname token that isn't just an IdentiferToken
ConcMethodDefs : ConcMethodDef ConcMethodDefs { $1 : $2}
                 | {- empty -} {[]}
ConcMethodDef : method var '(' CommaParam ')' ':' Type '{' Stmts '}' {ConcMethodDefRoot $2 $4 $7 $9}

AbsMethodDefs : AbsMethodDef AbsMethodDefs {$1 : $2}
                | {- empty -} {[]}

AbsMethodDef : method var '(' CommaParam ')' ':' Type ';' {AbsMethoddefRoot $2 $4 $7}


StructDef : struct var '{' CommaParam '}'{StructDefRoot $2 $4}

CommaParam : Param ',' CommaParam {$1 : $3}
           | Param {[$1]}
           | {- empty -} {[]}

Param : var ':' Type {ParamRoot $1 $3 }

Type :  Int {IntType}
       | Void {VoidType}
       | Boolean {BooleanType} 
       | Self {SelfType}
       | '(' Type ')' {ParenthType $2}
       | '(' CommaType ')' "=>" Type {HighOrderType $2 $5}
       | var {StructType $1}



CommaType : Type ',' CommaType {$1 : $3}
          | Type {[$1]}
          | {- empty -} {[]}            

             
AddOp : '+' {AddToken}
      | '-' {SubtractToken}
        
EqualsOp : "==" {EqualsToken}
         | "!=" {NotEqualToken}

LessThanOp : '<' {LessThanToken} 
           | '>' {GreaterThanToken}

MultOp : '*' {MultiplyToken}
       | '/' {DivideToken}



{
 --AST representation. The website didn't really give me a clear indicator on how to deal with * closures so I just did lists.
data Program = 
         Program [ProgramItem] [Stmt]
         deriving (Show,Eq)


data ProgramItem =
          StructDefLeaf StructDef
        | TraitDefLeaf TraitDef
        | ImplDefLeaf ImplDef
        | FuncDefLeaf FuncDef
          deriving (Show,Eq)

data Exp = ExpRoot EqualsExp
          deriving(Show,Eq)


data EqualsExp = EqualsExpRoot LessThanExp Token LessThanExp
             deriving (Show,Eq)

data LessThanExp =
    LessThanExpRoot AddExp Token AddExp
     deriving (Show,Eq)

data AddExp =
    AddExpRoot MultExp Token MultExp
    deriving (Show,Eq)

data MultExp =
    MultExpRoot CallExp Token CallExp
    deriving (Show,Eq)

data CallExp =
     CallExpRoot DotExp DotExp
   | DotEXPCallExpRoot DotExp
    deriving (Show,Eq)

data DotExp =
    DotExpRoot PrimaryExp Token PrimaryExp
    deriving (Show,Eq)

data PrimaryExp =
     IntRoot Int
   | VarRoot String
   | BoolRoot Bool
   | ParenExpRoot Exp
   | LowerSelfRoot
   | NewRoot String [StructActualParam]
   deriving (Show,Eq)


data StructActualParam = StructActualParamRoot String Exp
                        deriving (Show,Eq)

data Stmt =
     LetStmt Param Exp
   | AssignStmt String Exp
   | IfStmt Exp Stmt (Maybe Stmt)
   | WhileStmt Exp Stmt
   | BreakStmt 
   | PrintLnStmt Exp
   | BlockStmt [Stmt]
   | ReturnStmt (Maybe Exp)
   | ExpStmt Exp
    deriving (Show,Eq)

data StructDef = StructDefRoot String [Param]
                deriving (Show,Eq)
data TraitDef = TraitDefRoot String [AbsMethodDef]
                deriving (Show,Eq)
data ImplDef = ImplDefRoot String Type [ConcMethodDef]
                deriving (Show,Eq)
data FuncDef = FuncDefRoot String [Param] Type [Stmt]
                deriving (Show,Eq)

data ConcMethodDef
  = ConcMethodDefRoot String [Param] Type [Stmt]
    deriving (Show,Eq)

data AbsMethodDef
  = AbsMethoddefRoot String [Param] Type
    deriving (Show,Eq)

data Param
  = ParamRoot String Type
    deriving (Show,Eq)

data Type
 = IntType 
  | VoidType
  | BooleanType
  | SelfType
  | StructType String
  | ParenthType Type
  | HighOrderType [Type] Type
     deriving (Show,Eq)




parseError :: [Token] -> a1
parseError _ = error "Parse Error"

}

