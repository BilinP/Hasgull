{
module Parser.Parser
    ( 
      parseTokens
      
    ) where

    import Tokenizer.Tokenizer
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
Exp : EqualsExp {$1}

EqualsExp : LessThanExp EqualsOp LessThanExp {EqualsExpRoot $1 $2 $3}
            | LessThanExp {$1}

LessThanExp : AddExp LessThanOp AddExp {LessThanExpRoot $1 $2 $3}
            | AddExp {$1}

AddExp : MultExp AddOp MultExp {AddExpRoot $1 $2 $3}
         | MultExp {$1}

MultExp : DotExp MultOp DotExp {MultExpRoot $1 $2 $3}
          | DotExp {$1}

CallExp : DotExp '(' DotExp ')' {CallExpRoot $1 $3}
          | DotExp {$1}

DotExp : PrimaryExp '.' var {DotExpRoot $1 $3}
         | PrimaryExp {$1}

PrimaryExp: i {IntRoot $1}
            | var {VarRoot $1}
            | true {BoolRoot True}
            | false {BoolRoot False}
            | self {LowerSelfRoot}
            | '(' Exp ')' {ParenExpRoot $2}
            | new var '{' StructActualParams '}' {NewRoot $2 $4}

StructActualParams : StructActualParam ',' StructActualParams {$1 : $3}
                    | StructActualParam {[$1]}
                    | {- empty -} {[]}

StructActualParam : var ':' Exp  {StructActualParamRoot $1 $3}


Stmt : let Param '=' Exp ';' {LetStmt $2 $4}
       | var '=' Exp ';' {AssignStmt $1 $3}
       | if '(' Exp ')' Stmt else Stmt {IfStmt $3 $5 (Just $7)}
       | if '(' Exp ')' Stmt {IfStmt $3 $5 Nothing}
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
       | var {StructType}
       | '(' Type ')' {ParenthType $2}
       | '(' CommaType ')' "=>" Type {HighOrderType $2 $5}

CommaType : Type ',' CommaType {$1 : $3}
            | Type {[$1]}
            | {- empty -} {[]}            

             
AddOp : '+' {AddToken}
      | '-' {SubtractToken}
        
EqualsOp : "==" {EqualsToken}
         | "!=" {NotEqualsToken}

LessThanOp : '<' {LessThanToken} 
           | '>' {GreaterThanToken}

MultOp : '*' {MultiplyToken}
       | '/' {DivideToken}



{
 --AST representation. The website didn't really give me a clear indicator on how to deal with * closures so I just did lists.
  data Program = 
         Program [ProgramItem] [Stmt]


  data ProgramItem =
          StructDef StructDef
         |  TraitDef TraitDef
         | Impldef ImplDef
         | FuncDefItem FuncDef

 data Exp = EqualsExpRoot Exp Token Exp | LessThanExpRoot Exp Token Exp | AddExpRoot Exp Token Exp
   | CallExpRoot Exp Token Exp | DotExpRoot Exp Token Exp
   | IntRoot Int
   | VarRoot String
   | BoolRoot Bool
   | ParenExpRoot Exp
   | NewRoot String [StructActualParam]

 data StructActualParam = StructActualParamRoot String Exp

 data Stmt =
     LetStmt Param Exp
   | AssignStmt String Exp
   | IfStmtNode Exp Stmt (Maybe Stmt)
   | WhileStmt Exp Stmt
   | BreakStmt 
   | PrintLnStmt Exp
   | BlockStmt [Stmt]
   | ReturnStmt (Maybe Exp)
   | ExpStmt Exp

 data StructDef = StructDefRoot String [Param]
 data TraitDef = TraitDefRoot String [AbsMethodDef]
 data ImplDef = ImplDefRoot String Type [ConcMethodDef]
 data FuncDef = FuncDefRoot String [Param] Type [Stmt]

 data ConcMethodDef
  = ConcMethodDefRoot String [Param] Type [Stmt]

 data AbsMethodDef
  = AbsMethoddefRoot String [Param] Type

 data Param
  = ParamRoot String Type

 data Type
 = IntType 
  | VoidType
  | BooleanType
  | SelfType
  | StructType String
  | ParenthType Type
  | HighOrderType [Type] Type




parseError :: [Token] -> a1
parseError _ = error "Parse Error"

}

