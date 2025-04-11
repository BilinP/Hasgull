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

--Essentially just copied our grammar straight into here, mostly following the example on the Happy website
-- NOTE: the $1 essentially the value in the production rule, they must match.
program : programItems stmts {Program $1 $2}

programItems : {- empty -} {[]}
             | programItem programItems {$1 : $2}

stmts : {- empty -} { [] }
      | stmt stmts {$1 : $2}

programItem : structDef {StructDefLeaf $1}
            | traitDef {TraitDefLeaf $1}
            | implDef {ImplDefLeaf $1}
            | funcDef {FuncDefLeaf $1}

exp : equalsExp {Exp $1}

equalsExp : lessThanExp "==" lessThanExp {Equals $1 $3 }
          | lessThanExp "!=" lessThanExp {Equals $1 $3}
          | lessThanExp {Equals $1}

lessThanExp : addExp '<' addExp {LessThan $1 $3}
            | addExp '>' addExp {LessThan $1 $3}
            | addExp {LessThan $1}

addExp : multExp '+' multExp {Add $1 $3}
       | multExp '-' multExp {Add $1 $3}
       | multExp {Add $1}

multExp : callExp '*' callExp {Mult $1 $3}
        | callExp '/' callExp {Mult $1 $3}
        | callExp {Mult $1}


callExp : dotExp '('exp')'{Call $1 $3} --The grammer in Hasgull has there be a commaExp, but that doesn't exist in the slightest
        | dotExp {Call $1}

dotExp : primaryExp '.' var {Dot $1 $3}
       | primaryExp {Dot $1}

primaryExp : i {Int $1}
           | var {Var $1}
           | true {True }
           | false {False}
           | self {LowerSelf}
           | '(' exp ')' {$2}
           | new var '{' structActualParams '}' {New $1 $3}


structActualParams : {- empty -} {[]}
                   | structActualParam {[$1]}
                   | structActualParam ',' structActualParams {$1 : $3}

                   
structActualParam : var ':' exp {StructActualParamRoot $1 $3}

stmt : let param '=' exp ';' {LetStmt $2 $4}
     | var '=' exp ';' {AssignStmt $1 $3}
     | if '(' exp ')' stmt else stmt %prec LOWER_THAN_ELSE {IfStmt $3 $5 (Just $7)}
     | if '(' exp ')' stmt {IfStmt $3 $5 Nothing}
     | while '(' exp ')' stmt {WhileStmt $3 $5}
     | break ';' {BreakStmt}
     | println '(' exp ')' {PrintLnStmt $3}
     | '{' stmts '}' {BlockStmt $2}
     | return exp ';' {ReturnStmt (Just $2)}
     | return ';' {ReturnStmt Nothing}
     | exp ';' {ExpStmt $1}

funcDef : func var '(' commaParam ')' ':' type '{' stmts '}' {FuncDefRoot $2 $4 $7 $9}

implDef : impl var for type '{' concMethodDefs '}' {ImplDefRoot $2 $4 $6}

traitDef : trait var '{' absMethodDefs '}' {TraitDefRoot $2 $4}

concMethodDefs : concMethodDef concMethodDefs { $1 : $2}
               | {- empty -} {[]}

concMethodDef : method var '(' commaParam ')' ':' type '{' stmts '}' {ConcMethodDefRoot $2 $4 $7 $9}

absMethodDefs : absMethodDef absMethodDefs {$1 : $2}
              | {- empty -} {[]}

absMethodDef : method var '(' commaParam ')' ':' type ';' {AbsMethoddefRoot $2 $4 $7}

structDef : struct var '{' commaParam '}' {StructDefRoot $2 $4}

commaParam : param ',' commaParam {$1 : $3}
           | param {[$1]}
           | {- empty -} {[]}

param : var ':' type {ParamRoot $1 $3}

type : Int {IntType}
     | Void {VoidType}
     | Boolean {BooleanType}
     | Self {SelfType}
     | '(' type ')' {ParenthType $2}
     | '(' commaType ')' "=>" type {HighOrderType $2 $5}
     | var {StructType $1}

commaType : type ',' commaType {$1 : $3}
          | type {[$1]}
          | {- empty -} {[]}


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

data Exp = Exp EqualsExp -- Hmmmm

data EqualsExp = Equals LessThanExp LessThanExp
            | Equals LessThanExp LessThanExp
            | Equals LessThanExpExp
            deriving(Show,Eq)

data LessThan = LessThan AddExp AddExp
              | LessThan AddExp AddExp
              | LessThan AddExp
              deriving(Show,Eq)

data AddExp = Add MultExp MultExp
            | Add MultExp MultExp
            | Add MultiExp
            deriving(Show,Eq)

data MultExp = Mult CallExp CallExp
             | Mult CallExp CallExp
             | Mult CallExp
             deriving(Show,Eq)

data CallExp = Call DotExp Exp
             | Call DotExp
             deriving(Show,Eq)
data DotExp = Dot PrimaryExp String 
            | Dot PrimaryExp
            deriving(Show,Eq)

data PrimaryExp = Int Int
                | Var String
                | True
                | False
                | LowerSelf
                | Par Exp -- IDK for the '(' exp ')' rule
                | New String StructActualParams

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

